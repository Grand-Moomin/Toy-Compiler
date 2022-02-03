#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

#define TOKEN_NAME_SIZE (1<<7)
//#define __DEBUG__

#ifdef __DEBUG__
#define dbg_printf printf
#else
#define dbg_printf
#endif

/*--- Tokenizer ---*/
class tokenizer {
    protected:
        int line;       //current parsing line No.
        int pool_size;
        char *curr_pos;         //Position of the current token.
        char *last_token_pos;   //Position of the last token, used in case of retrogression.
        int token;      //current token
        int token_val;  //current token value (Meaningful only for numbers)
        struct symbol {
            int token;          //A global symbol is either a global variable (Id), a function (Id), or a keyword
            int hash;
            char name[TOKEN_NAME_SIZE];    //Identifier name length should be less than 127 bytes.
            int symbol_type;               //cf. symbol_types
            union {
                int ret_val_type;   //Return value type of a function
                int var_type;       //Variable type
            } data_type;
            union {
                long long abs_addr;         //Absolute addr of global identifiers
                long long rel_stack_pos;    //Relative position from current stack base pointer, i.e. ebp. 
                                            //Ex: relative pos -1 stands for absolute addr (ebp - 8) where the first local variable is stored. 
                                            /* --------IMPORTANT: Stack frame convention for our 64 bit virtual machine---------
                                                Address     : Contents                  :rel_stack_pos
                                                ...
                                                ebp + 24    : 2nd argument              : 3
                                                ebp + 16    : 1st argument              : 2
                                                ebp + 8     : return eip                : 1
                                                ebp         : previous stack frame ebp  : 0
                                                ebp - 8     : 1st local variable        : -1
                                                ebp -16     : 2nd local variable        : -2
                                                ...
                                            */
            } addr;
        } *global_symbols, *local_symbols;  //Storing identifiers. Local symbols only valid for current function being parsed
        struct symbol *curr_symbol;         //Current parsed symbol
        struct symbol *curr_local_symbol;   //Current parsed local symbol
        bool local_context;                 //If current token is inside a function body other than the local variable definition section, this flag will be true.
        bool local_variable_context;        //If current token is in local variable definition section, this flag will be true.
        bool argument_context;              //If current token is in a function parameter list, this flag will be true.
        long long rel_pos;                  //relative position of local variables from ebp

        enum token_types {Others = 256, Num, Id/*Identifier, either a variable or a function*/, 
                        If, Else, Int, Return, While, Main, Print, /*Keywords*/
                        Assign, Lor/*Logical or*/, Land/*Logical and*/, /*operators*/
                        Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Add, Sub, Mul, Div, Mod, Inc, Dec, /*Operators*/
                        L_Sq_Bracket /*Left square bracket*/};
        enum symbol_types {Loc = 128, Glo, Func, Sys};
        enum data_types {Integer = 1, Pointer}; 

    public:
        tokenizer(char *orig_src, int pool_size = 4096*100)
        {
            curr_symbol = global_symbols = (struct symbol *)malloc(pool_size);
            local_symbols = (struct symbol *)malloc(pool_size);
            this->pool_size = pool_size;
            memset(global_symbols, 0, pool_size);
            memset(local_symbols, 0, pool_size);
            init_keywords();

            curr_pos = orig_src;
            token = -1;
            token_val = 0;
            line = 1;
            local_context = local_variable_context = argument_context = false;
        }
        ~tokenizer()
        {
            free((void *)global_symbols);
            free((void *)local_symbols);
        }

        inline int get_curr_token()
        {
            return token;
        }
        inline int get_curr_token_value()
        {
            return token_val;
        }

        inline int is_var_pointer(struct symbol* symbol){
            return symbol->data_type.var_type > Integer;
        }
        int next_token()
        {
            char curr_ch;
            char *last_pos = last_token_pos = curr_pos;
            int hash;
            int id_len;

            while(curr_ch = *curr_pos++){
                if(curr_ch == ' ' || curr_ch == '\t'){
                    last_pos++;
                    continue;
                }
                else if(curr_ch == '\n'){
                    last_pos++;
                    line++;
                    continue;
                }
                else if(is_alphabet(curr_ch) || curr_ch == '_'){
                    hash = curr_ch;
                    curr_ch = *curr_pos;
                    while(is_alphabet(curr_ch) || is_single_digit(curr_ch) || curr_ch == '_'){
                        hash = hash * 147 + curr_ch;
                        curr_ch = *++curr_pos;
                    }
                    id_len = curr_pos - last_pos;

                    if(local_context == true){
                        //Firstly, search in current local symbol table.
                        if(!lookup_symbol(false, last_pos, id_len, hash)){
                            //Then, search in global symbol table.
                            if(!lookup_symbol(true, last_pos, id_len, hash)){
                                //Current token exists in neither global nor local symbol table, so the last resort is to prompt an error message.
                                char token_name[TOKEN_NAME_SIZE];
                                memcpy(token_name, last_pos, ((id_len>=TOKEN_NAME_SIZE)? TOKEN_NAME_SIZE: id_len));
                                token_name[id_len] = 0;
                                printf("Variable %s is not declared.\n", token_name);
                                exit(-1);
                            }
                        }
                    }
                    else if(argument_context == true || local_variable_context == true){
                        if(!lookup_symbol(true, last_pos, id_len, hash) || curr_symbol->token == Id){
                            //Function parameters should be considered as local variables
                            if(!lookup_symbol(false, last_pos, id_len, hash)){
                                add_curr_symbol(false, last_pos, id_len, hash, Id);
                            }
                        }                       
                    }
                    else{
                        if(!lookup_symbol(true, last_pos, id_len, hash)){
                            add_curr_symbol(true, last_pos, id_len, hash, Id);
                        }
                    }
                    token_val = 0;
                    return token = curr_symbol->token;
                }
                else if(is_single_digit(curr_ch)){
                    token_val = curr_ch - '0';
                    curr_ch = *curr_pos;
                    while(is_single_digit(curr_ch)){
                        token_val = token_val * 10 + curr_ch - '0';
                        curr_ch = *++curr_pos;
                    }
                    return token = Num;
                }
                else if(curr_ch == '/'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '/'){
                        while(curr_ch && curr_ch != '\n'){
                            curr_ch = *curr_pos++;
                        }
                        return token = Others;
                    }
                    else{
                        return token = Div;
                    }
                }
                else if(curr_ch == '*'){
                    return token = Mul;
                }
                else if(curr_ch == '='){
                    curr_ch = *curr_pos;
                    if(curr_ch == '='){
                        curr_pos++;
                        return token = Eq;
                    }
                    else{
                        return token = Assign;
                    }
                }
                else if(curr_ch == '!'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '='){
                        curr_pos++;
                        return token = Ne;
                    }
                    else{
                        goto error_exit;
                    }
                }
                else if(curr_ch == '>'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '='){
                        curr_pos++;
                        return token = Ge;
                    }
                    else{
                        return token = Gt;
                    }
                }
                else if(curr_ch == '<'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '='){
                        curr_pos++;
                        return token = Le;
                    }
                    else{
                        return token = Lt;
                    }
                }
                else if(curr_ch == '^'){
                    return token = Xor;
                }
                else if(curr_ch == '|'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '|'){
                        curr_pos++;
                        return token = Lor;
                    }
                    else{
                        return token = Or;
                    }
                }
                else if(curr_ch == '&'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '&'){
                        curr_pos++;
                        return token = Land;
                    }
                    else{
                        return token = And;
                    }
                }
                else if(curr_ch == '+'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '+'){
                        curr_pos++;
                        return token = Inc;
                    }
                    else{
                        return token = Add;
                    }
                }
                else if(curr_ch == '-'){
                    curr_ch = *curr_pos;
                    if(curr_ch == '-'){
                        curr_pos++;
                        return token = Dec;
                    }
                    else{
                        return token = Sub;
                    }
                }
                else if(curr_ch == '%'){
                    return token = Mod;
                }
                else if(curr_ch == '['){
                    return token = L_Sq_Bracket;
                }
                else if(curr_ch == '~' || curr_ch == ';' || curr_ch == '{' || curr_ch == '}' || \
                        curr_ch == '(' || curr_ch == ')' || /*curr_ch == '[' ||*/ curr_ch == ']' || \
                        curr_ch == ','){
                            return token = curr_ch;
                }
                else{
                error_exit:
                    token_val = 0;
                    return token = -1; //Unknown token
                }
            }
            token_val = 0;
            return token = 0; //End of stream
        }
        bool match(int required_token)
        {
            if(token == required_token){
                next_token();
                return true;
            }
            else{
                printf("Line: %d : Token mismatch: token = %c(%d), required token = %c(%d)\n", line, token, token, required_token,required_token);
                exit(-1);
            }
        }

        //Tokenizer test run
        void run()
        {
            next_token();
            printf("Token: %d:%c, Val: %d\n",token, token, token_val);
            while(token > 0){
                next_token();
                printf("Token: %d:%c, Val: %d\n",token, token, token_val);
            }
        }

    private:
        inline bool is_alphabet(char ch){
            return ((ch >= 'A' && ch <= 'Z')||(ch >= 'a' && ch <= 'z'));
        }
        inline bool is_single_digit(char ch){
            return (ch >= '0' && ch <= '9');
        }    
        inline void init_keywords()
        {
            int token_tmp = If;
            char keywords[] = "if else int return while main printf"; //Should be the same order as in "enum token_types".
            curr_pos = keywords;
            next_token();
            while(token > 0){
                curr_symbol->token = token_tmp++;
                next_token();
            }
        }
        inline bool lookup_symbol(bool global_flag, char *start, int len, int hash)
        {
            curr_symbol = (global_flag == true) ? global_symbols : local_symbols;
            while(curr_symbol->token){
                if(curr_symbol->hash == hash && !memcmp(curr_symbol->name, start, len)){
                    return true;
                }
                curr_symbol++;
            }
            //Here "curr_symbol" is pointing to the next empty slot of symbol table
            return false;
        }
        inline bool add_curr_symbol(bool global_flag, char *start, int len, int hash, int token)
        {
            int offset;
            char *symbols_base;

            if(len >= TOKEN_NAME_SIZE)
                return false;
            
            symbols_base = (global_flag == true) ? (char *)global_symbols : (char *)local_symbols;
            offset = (char *)curr_symbol - symbols_base;
            if(offset < 0 || offset >= pool_size){
                return false;
            }

            while(curr_symbol->token)
                curr_symbol++;
            curr_symbol->token = token;
            curr_symbol->hash = hash;
            memcpy(curr_symbol->name, start, len);
            curr_symbol->name[len] = 0;
            curr_symbol->symbol_type = (global_flag == true) ? Glo : Loc;
            return true;
        }
    protected:
        inline void clear_local_symbols(){
            memset(local_symbols, 0, pool_size);
        }
        inline void enter_local_context(){
            local_context = true;            
        }
        inline void exit_local_context(){
            local_context = false;
            clear_local_symbols();
        }
        inline void enter_local_variable_context(){
            local_variable_context = true;
            rel_pos = -1;//start pos of local variables relative to ebp
        }
        inline void exit_local_variable_context(){
            local_variable_context = false;
            //Obnoxious corner case: 
            //Exculde the case that the symbol of global variable or function is erroneously treated as a local.
            /*
            Due to the fact that legitimate local variables should have a valid offset at the end of local variable declaration section,
            the symbol with no valid offset at the beginning of statement section should be a global variable or function, but has been misinterpreted as a local one,
            as the parsing of first statement was still in local variable declaration section (where flag "local_variable_context" is set.).
            To solve this, we exclude the token from local symbol table, regress to the last symbol, 
            and reparse it after setting flag "local_variable_context" to false, 
            which forces the "next_token()" function to search the token in the global symbol table if it is originally a global variable or function.
            */
            if(!curr_symbol->addr.rel_stack_pos && curr_symbol->symbol_type == Loc){
                curr_symbol->token = 0; //Nullify the token in local symbol table
                curr_pos = last_token_pos;//Take a step back to reinterprete the last token. 
                next_token(); //Reparsing the symbol. As all context flags are false at this point, search the token in global symbol table.
            }
        }
        inline void enter_argument_context(){
            argument_context = true;
            rel_pos = 2;//start pos of arguments relative to ebp
        }
        inline void exit_argument_context(){
            argument_context = false;
        }
    public:
        void iterate_global_symbols()
        {
#ifdef __DEBUG__
            curr_symbol = global_symbols;
            while(curr_symbol->token){
                printf("%s\t%d\t%llx\t%d\t%d\n", curr_symbol->name, curr_symbol->token, curr_symbol->addr.abs_addr,curr_symbol->symbol_type, curr_symbol->data_type.var_type);
                curr_symbol++;
            }
#endif
        }
        void iterate_local_symbols()
        {
#ifdef __DEBUG__
            curr_local_symbol = local_symbols;
            while(curr_local_symbol->token){
                printf("%s\t%d\t%lld\t%d\n", curr_local_symbol->name, curr_local_symbol->token, curr_local_symbol->addr.rel_stack_pos, curr_local_symbol->data_type.var_type);
                curr_local_symbol++;
            }
#endif
        }
};

//Instruction definition for our tiny virtual machine
/*
    Presumptions: 
        MOV instruction is valid only for registers;
        LD, ST instructions is used for memory access with absolute address;
        LDSFO, STSFO instructions is designed for memory access with relative offset position in current stack frame, of which the base address is stored in ebp. 
        DEREF: ex. DEREF EAX, EBX means MOV EAX, [EBX].
        DEREF2: ex. DEREF2 EBX, EAX means MOV [EBX], EAX.
*/
enum instructions{
    JMP = 1, CALL, RET, PUSH, POP, MOV, MOVIMM, LD, ST, LDSFO/*Load by stack frame offset*/, STSFO, /*Store by stack frame offset*/ /*General instructions*/ DEREF, DEREF2, 
    OR, XOR, AND, ADD, SUB, MUL, DIV, MOD, INC, DEC, /*Arithmetic instructions*/
    CMP/*Compare before any conditional jump*/, 
    JA /*if greater than*/, JAE /*if greater than or equal to*/, JB /*if less than*/, JBE/*if less than or equal to*/, JZ, JNZ, JE, JNE, 
    EIP, ESP, EBP, EAX, EBX, /*Registers*/
    EXIT, PRINT
};

/*--- Code generator ---*/
#define CODE_DEF(fun, ...) \
        inline void fun()          \
        {                          \
            long long instructions[] = {__VA_ARGS__};\
            for(int i = 0; i < sizeof(instructions)/sizeof(instructions[0]); i++){      \
                *text++ = instructions[i];  \
                text_bytes += sizeof(long long); }\
        }

#define CODE_DEF1(fun, type, param, ...) \
        inline void fun(type param)          \
        {                          \
            long long instructions[] = {__VA_ARGS__};\
            for(int i = 0; i < sizeof(instructions)/sizeof(instructions[0]); i++){      \
                *text++ = instructions[i];  \
                text_bytes += sizeof(long long); }\
        }

/*  At first, pop the first element in the stack to reg_dest;
    Do the operation: the result is in reg_dest;
    Finally, push reg_dest, what conclude the operation on the first element of the original stack.*/
#define CODE_DEF_STACK_FIRST_ELEM_OP(op, reg_dest, reg_src) \
        void op##_stack_first_elem_##reg_src(){ \
            pop_##reg_dest();    \
            op##_##reg_dest##_##reg_src();\
            push_##reg_dest();}

//Here, relation includes == != > < >= <=
#define CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(relation, conditional_jmp)\
        void relation##_stack_first_elem_eax(){ \
            pop_ebx();  \
            cmp_ebx_eax();  \
            conditional_jmp((long long)(text+7));    \
            movimm_ebx(0);  \
            jmp((long long)(text+5));   \
            movimm_ebx(1);  \
            push_ebx(); }

//Here, relation includes || &&
#define CODE_DEF_STACK_FIRST_ELEM_RELATION_OP2(rel, rel_cal) \
        void rel##_stack_first_elem_eax(){  \
            pop_ebx();  \
            rel_cal##_ebx_eax();    \
            movimm_eax(0);  \
            cmp_ebx_eax();  \
            jnz((long long)(text+7));   \
            movimm_ebx(0);  \
            jmp((long long)(text+5));   \
            movimm_ebx(1);  \
            push_ebx(); }

class code_generator{
    protected:
        long long *text, *orig_text;
        long long *data;
        long long *main_func_addr;
        long long text_bytes;
        long long data_bytes;

    public:
        code_generator(char *text, char *data)
        {
            this->text = this->orig_text = (long long *)text;
            this->data = (long long *)data;
            text_bytes = data_bytes = 0;
            main_func_addr = NULL;
        }
        inline long long get_text_bytes(){return text_bytes = (text - orig_text)*sizeof(long long);}
        inline long long get_data_bytes(){return data_bytes;}
        inline void set_main_func_addr(long long *main_func_addr){this->main_func_addr = main_func_addr;}
        inline long long *get_main_func_addr(){return main_func_addr;}
        //EXIT
        CODE_DEF(exit_prog, EXIT);
        //PRINT EAX
        CODE_DEF(print_eax, PRINT, EAX);
        //PUSH EBP
        CODE_DEF(push_ebp, PUSH, EBP);
        //POP EBP
        CODE_DEF(pop_ebp, POP, EBP);
        //MOV ESP, EBP
        CODE_DEF(mov_esp_ebp, MOV, ESP, EBP);
        //MOV EBP, ESP
        CODE_DEF(mov_ebp_esp, MOV, EBP, ESP);
        //PUSH EAX
        CODE_DEF(push_eax, PUSH, EAX);
        //POP EAX
        CODE_DEF(pop_eax, POP, EAX);
        //PUSH EBX
        CODE_DEF(push_ebx, PUSH, EBX);
        //POP EBX
        CODE_DEF(pop_ebx, POP, EBX);
        //MOV EAX, EBX
        CODE_DEF(mov_eax_ebx, MOV, EAX, EBX);
        //MOV EBX, EAX
        CODE_DEF(mov_ebx_eax, MOV, EBX, EAX);
        //DEREF EAX, EBX -> MOV EAX, [EBX]
        CODE_DEF(deref_eax_ebx, DEREF, EAX, EBX);
        //DEREF2 EAX, EBX -> MOV [EBX], EAX
        CODE_DEF(deref2_ebx_eax, DEREF2, EBX, EAX);
        //RET
        CODE_DEF(ret, RET);
        //CALL abs_addr
        CODE_DEF1(call, long long, addr, CALL, addr);
        //JMP abs_addr
        CODE_DEF1(jmp, long long, addr, JMP, addr);
        //MOVIMM EAX, imm
        CODE_DEF1(movimm_eax, long long, imm, MOVIMM, EAX, imm);
        //MOVIMM EBX, imm
        CODE_DEF1(movimm_ebx, long long, imm, MOVIMM, EBX, imm);
        //ADD EBX, EAX
        CODE_DEF(add_ebx_eax, ADD, EBX, EAX);
        //SUB EBX, EAX
        CODE_DEF(sub_ebx_eax, SUB, EBX, EAX);
        //MUL EBX, EAX
        CODE_DEF(mul_ebx_eax, MUL, EBX, EAX);
        //MUL EAX, EBX
        CODE_DEF(mul_eax_ebx, MUL, EAX, EBX);
        //DIV EBX, EAX
        CODE_DEF(div_ebx_eax, DIV, EBX, EAX);
        //MOD EBX, EAX
        CODE_DEF(mod_ebx_eax, MOD, EBX, EAX);
        //AND EBX, EAX
        CODE_DEF(and_ebx_eax, AND, EBX, EAX);
        //OR EBX, EAX
        CODE_DEF(or_ebx_eax, OR, EBX, EAX);
        //XOR EBX, EAX
        CODE_DEF(xor_ebx_eax, XOR, EBX, EAX);
        //INC EBX
        CODE_DEF(inc_ebx, INC, EBX);
        //DEC_EBX
        CODE_DEF(dec_ebx, DEC, EBX);
        //ADD ESP, EBX
        CODE_DEF(add_esp_ebx, ADD, ESP, EBX);
        //ADD ESP, imm
        void add_esp_imm(long long imm){
            movimm_ebx(imm);
            add_esp_ebx();
        }
        //SUB ESP, EBX
        CODE_DEF(sub_esp_ebx, SUB, ESP, EBX);
        //SUB ESP, imm
        void sub_esp_imm(long long imm){
            movimm_ebx(imm);
            sub_esp_ebx();
        }

        //PUSH imm
        void push_imm(long long imm){
            movimm_ebx(imm);
            push_ebx();
        }

        //ADD EBX, EBP
        CODE_DEF(add_ebx_ebp, ADD, EBX, EBP);

        //MOV EAX, [Abs_addr]
        CODE_DEF1(mov_global_var_to_eax, long long, addr, LD, EAX, addr);
        //MOV EBX, [Abs_addr]
        CODE_DEF1(mov_global_var_to_ebx, long long, addr, LD, EBX, addr);
        //PUSH [Abs_addr]
        void push_global_var(long long abs_addr){
            mov_global_var_to_ebx(abs_addr);
            push_ebx();
        }
        //MOV EAX, [ebp]Rel_offset //Rel_offset is offset relative to current ebp pointer.
        CODE_DEF1(mov_local_var_to_eax, long long, offset, LDSFO, EAX, offset);
        //MOV EBX, [ebp]Rel_offset //Rel_offset is offset relative to current ebp pointer.
        CODE_DEF1(mov_local_var_to_ebx, long long, offset, LDSFO, EBX, offset);
        //PUSH [Rel_offset]
        void push_local_var(long long offset){
            mov_local_var_to_ebx(offset);
            push_ebx();
        }
        //MOV [Abs_addr], EAX
        CODE_DEF1(mov_eax_to_global_var, long long, addr, ST, addr, EAX);
        //MOV [Abs_addr], EBX
        CODE_DEF1(mov_ebx_to_global_var, long long, addr, ST, addr, EBX);
        //MOV [ebp]Rel_offset, EAX
        CODE_DEF1(mov_eax_to_local_var, long long, offset, STSFO, offset, EAX);
        //MOV [ebp]Rel_offset, EBX
        CODE_DEF1(mov_ebx_to_local_var, long long, offset, STSFO, offset, EBX);

        //ADD [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(add, ebx, eax); 
        //Note: "ebx" used as intermediate register to execute the operation,
        //      as we suppose memory addressing is unavailable for calculation instructions on our virtual machine.
        //SUB [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(sub, ebx, eax);
        //MUL [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(mul, ebx, eax);
        //DIV [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(div, ebx, eax);
        //MOD [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(mod, ebx, eax);
        //AND [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(and, ebx, eax);
        //OR [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(or, ebx, eax);
        //XOR [esp], EAX
        CODE_DEF_STACK_FIRST_ELEM_OP(xor, ebx, eax);

        //CMP ebx, eax
        CODE_DEF(cmp_ebx_eax, CMP, EBX, EAX);
        //JZ [addr]
        CODE_DEF1(jz, long long, addr, JZ, addr);
        //JNZ [addr]
        CODE_DEF1(jnz, long long, addr, JNZ, addr);
        //JA [addr]
        CODE_DEF1(ja, long long, addr, JA, addr);
        //JAE [addr]
        CODE_DEF1(jae, long long, addr, JAE, addr);
        //JB [addr]
        CODE_DEF1(jb, long long, addr, JB, addr);
        //JBE [addr]
        CODE_DEF1(jbe, long long, addr, JBE, addr);

        /*Example to explain the jumping postion: 
        void eq_stack_first_elem_eax(){
            pop_ebx();
            cmp_ebx_eax();
            jz((long long)(text+7)); //Jump to "MOVIMM ebx 1" if ebx equals to eax and there are totally 7 ("JZ [addr]; MOVIMM EBX, 0; JMP [addr2]") position ahead. 
            movimm_ebx(0);
            jmp((long long)(text+5));//Jump to "PUSH ebx" if ebx does not equal to eax and there are totally 5 ("JMP [addr2]; MOV EBX, 1") position ahead.
            movimm_ebx(1);
            push_ebx();
        }*/
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(eq, jz);
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(ne, jnz);
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(lt, jb);
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(gt, ja);
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(le, jbe);
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP(ge, jae);

        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP2(lor, or);
        CODE_DEF_STACK_FIRST_ELEM_RELATION_OP2(land, and);
};


/*--- Parser ---*/

/* Macros to implement 
        PUSH [var]
        POP [var]
        MOV eax|ebx, [var]  
        MOV [var], eax|ebx. 
*/
#define var_to_reg(symbol, action, _to_reg) { \
            if(symbol->symbol_type == Glo)  \
                action##_global_var##_to_reg(symbol->addr.abs_addr);  \
            else if(symbol->symbol_type == Loc)   \
                action##_local_var##_to_reg(symbol->addr.rel_stack_pos); }
#define push_var(symbol) var_to_reg(symbol, push, )
#define mov_var_to_eax(symbol) var_to_reg(symbol, mov, _to_eax)
#define mov_var_to_ebx(symbol) var_to_reg(symbol, mov, _to_ebx)

#define reg_to_var(symbol, action, _reg_to) {\
            if(symbol->symbol_type == Glo) \
                action##_reg_to##_global_var(symbol->addr.abs_addr);\
            else if(symbol->symbol_type == Loc) \
                action##_reg_to##_local_var(symbol->addr.rel_stack_pos);}
#define pop_var(symbol) reg_to_var(symbol, pop,)
#define mov_eax_to_var(symbol) reg_to_var(symbol, mov, _eax_to)
#define mov_ebx_to_var(symbol) reg_to_var(symbol, mov, _ebx_to)

/*Our parser generates virtual machine code while parsing the source.*/
class parser : public tokenizer, public code_generator{
    private:
        long long *place_holder_addr; //text addr where main function addr should be stored.
        int expr_type; //Data type of the most recent parsed exprssion is stored.

    public:
        parser(char *text, char *data, char *orgi_src = NULL, int pool_size = 4096*100) : tokenizer(orgi_src, pool_size), code_generator(text, data)
        {
            place_holder_addr = NULL;
            program_start_section();//We put the entry point of program at the beginning.
        }

    private:
        inline void program_start_section()
        {
            //We suppose our main function takes no arguments here. 
            //CALL main
            call((long long)get_main_func_addr());  
            //An alternative is "JMP main", 
            //but the return address in the stack should be properly initialized, for example, *--esp = addr_of_exit_instruction.
            //See the following snippet.
            /*movimm_ebx((long long)(text+7));  //Note: Total length of instructions {MOVIMM EBX imm \n PUSH EBX \n JMP main} is 7.
            push_ebx();
            jmp((long long)get_main_func_addr());*/

            //As the source file has yet to be parsed, we save the text addr in which the main function addr should be stored.
            place_holder_addr = text - 1;

            //EXIT
            exit_prog();
        }

    public:
        void run()
        {
            /* 
                program ::= {global_declaration}+ 
            */
            next_token();
            while(token > 0){
                global_declaration();
            }
        }

    protected:
        void global_declaration()
        {
            /*
                global_decl ::= variable_decl | function_decl
                variable_decl ::= int {'*'}+ id {',' {'*'}+ id}+ ';'
                function_decl ::= int id|main '(' parameter_decl ')' '{' body_decl '}'
                parameter_decl ::= int id {',' int id}+ | empty
                body_decl ::= {variable_decl}+ {statement}+
            */
            if(!function_decl()){
                variable_decl();
            }
        }

        void function_decl_helper(struct symbol *curr_symbol, long long* text, bool is_main, int ret_type)
        {
            if(curr_symbol->addr.abs_addr){
                printf("Duplicated definition of global symbol %s at line %d.\n", curr_symbol->name, line);
                exit(-1);
            }
            curr_symbol->symbol_type = Func;
            curr_symbol->data_type.ret_val_type = ret_type;
            curr_symbol->addr.abs_addr = (long long)text;
            if(is_main == true){    //If we get main function definition, we store its addr to the place holder reserved for it.
                set_main_func_addr(text);
                if(place_holder_addr)
                    *place_holder_addr = (long long)text;
            }
            next_token();
            parameter_decl();
            match(')');
            match('{');
            body_decl();
            match('}');
        }

        bool function_decl()
        {
            char *start_pos = curr_pos;
            int ret_val_type = Integer; //Default return value type 'int'

            match(Int);
            while(token == Mul){
                ret_val_type += Pointer;
                next_token();
            }

            if(token == Main){
                next_token();
                if(token == '('){
                    function_decl_helper(curr_symbol, text, true, ret_val_type);
                    return true;
                }
                else{
                    printf("\"main\" can only be defined as function.");
                    exit(-1);
                }                
            }
            else if(token == Id){
                next_token();
                if(token == '('){
                    function_decl_helper(curr_symbol, text, false, ret_val_type);
                    return true;
                }
                else{
                    //Then it must be variable
                    token = Int;
                    curr_pos = start_pos; //rewind back to start position before function parsing
                    return false; 
                }
            }
            else{
                token = Int;
                curr_pos = start_pos;
                return false;
            }
        }

        void parameter_decl()
        {
            int arg_type;
            enter_argument_context();
            if(token == ')'){
                //function without an argument
                exit_argument_context();
                return;
            }
            else if(token == Int){
                arg_type = Integer; //Default type 'int'
                next_token();
                while(token == Mul){
                    arg_type += Pointer;
                    next_token();
                }
                while(1){
                    match(Id);
                    if(curr_symbol->addr.rel_stack_pos){
                        printf("Redefined argument: %s at line %d\n",curr_symbol->name, line);
                        exit(-1);
                    }
                    curr_symbol->addr.rel_stack_pos = rel_pos;
                    curr_symbol->data_type.var_type = arg_type;
                    rel_pos++;
                    if(token != ',' || token == ')')
                        break;
                    next_token();
                    match(Int);
                }
            }
            else{
                printf("Illegal function argument declaration at line %d\n",line);
                exit(-1);
            }
            exit_argument_context();
        }

        bool variable_decl()
        {
            int var_type;
            if(token != Int) //Not variable declaration as we assume only type 'int' supported.
                return false;
            
            next_token();
            while(1){
                var_type = Integer; //Default type 'int'
                while(token == Mul){
                    var_type += Pointer;
                    next_token();
                }
                match(Id);
                if(curr_symbol->symbol_type == Loc){
                    if(curr_symbol->addr.rel_stack_pos){
                        printf("Redefined local variable: %s at line %d\n",curr_symbol->name, line);
                        exit(-1);
                    }
                    curr_symbol->addr.rel_stack_pos = rel_pos;
                    rel_pos--;
                }
                else if(curr_symbol->symbol_type == Glo){
                    curr_symbol->addr.abs_addr = (long long)data;
                    data += sizeof(long long);
                    data_bytes += sizeof(long long);
                }
                curr_symbol->data_type.var_type = var_type;
                if(token == ';')
                    break;
                match(',');
            }
            match(';');
            return true;
        }

        void body_decl()
        {
            if(token == '}')
                return;
            
            enter_local_variable_context();
            while(variable_decl());
            exit_local_variable_context();
            iterate_local_symbols();

            enter_local_context();
            create_new_stack_frame();
            //Statement
            while(statement());
            restore_old_stack_frame();
            exit_local_context();
        }

        void create_new_stack_frame(){
            dbg_printf("PUSH EBP\n");
            push_ebp();
            dbg_printf("MOV EBP, ESP\n");
            mov_ebp_esp();
            long long num_of_local_var = -rel_pos - 1;
            if(num_of_local_var){
                dbg_printf("SUB ESP, %lld\n", num_of_local_var*sizeof(long long));
                sub_esp_imm(num_of_local_var*sizeof(long long));
            }
        }
        void restore_old_stack_frame(){
            dbg_printf("MOV ESP, EBP\n");
            mov_esp_ebp();
            dbg_printf("POP EBP\n");
            pop_ebp();
            dbg_printf("RET\n");
            ret();
        }

        bool statement()
        {    
            /*
                statement ::= expression ';'
                statement ::= '{' {statement}+ '}'
                statement ::= if '(' expression ')' statement [else statement]
                statement ::= while '(' expression ')' statement
                statement ::= return expression ';'
                statement ::= ';' //empty statement
            */
            long long *label_a, *label_b;

            if(token == ';'){
                next_token();
                return true;
            }
            else if(token == '{'){
                next_token();
                while(statement());
                match('}');
                return true;
            }
            else if(token == If){
                next_token();
                match('(');
                expression(Assign);
                match(')');
                //Compare the evaluated expression return value (in eax) with 0
                movimm_ebx(0);
                cmp_ebx_eax();
                //JZ [next_pos_of_the_if_statement_end]
                dbg_printf("JZ [if_statement_end]\n");
                *text++ = JZ;
                label_a = text++;
                statement();
                if(token == Else){
                    //JMP [next_pos_of_the_else_statement_end]
                    dbg_printf("JMP [else_statement_end]\n");
                    *text++ = JMP;
                    label_b = text++;
                }
                *label_a = (long long)text;
                if(token == Else){
                    next_token();
                    statement();
                    *label_b = (long long)text;
                }
                return true;
            }
            else if(token == While){
                label_a = text;
                next_token();
                match('(');
                expression(Assign);
                match(')');
                //Compare the evaluated expression return value (in eax) with 0
                movimm_ebx(0);
                cmp_ebx_eax();
                //JZ [next_pos_of_the_while_statement_end]
                dbg_printf("JZ [while_statement_end]\n");
                *text++ = JZ;
                label_b = text++;
                statement();
                //JMP [start_pos_of_the_while_statement]
                dbg_printf("JMP [while_statement_start]");
                jmp((long long)label_a);
                *label_b = (long long)text;
                return true;
            }
            else if(token == Return){
                next_token();
                expression(Assign);
                match(';');
                restore_old_stack_frame();
                return true;
            }
            else if(expression(Assign)){
                match(';');
                return true;
            }
            return false;
        }

        bool expression(int precedence){
            /*
                expression ::= id | num | func_call
                func_call  ::= id '(' [{expression ','}+ expression] ')'    //Fucntion call
                expression ::= id '=' expression                            //Assignment
                expression ::= {'--' | '++' id} | {id '--' | '++' }         //Increment & decrement
                expression ::= id {'[' expression ']'}+                     //Pointer dereference (i.e, Array access.)
                expression ::= '(' expression ')'
                expression ::= '-' | '+' | {'*'}+ | '&' expression          //Minus, plus, pointer dereference, address retrieving. (ex. -id +id **id &id)
                expression ::= id | func_call | num operator expression     ->    Precedence climbing
            */
            struct symbol *var_symbol = NULL;
            int imm_num;
            bool push_flag = false;
            long long num_of_arguments = 0;
            bool pointer_deref_flag = false;
            int first_stack_elem_type; //Data type of the "first stack element" from the perspective of current recursion.

            if(token == Id){
                //save current symbol for further use such as assignment and calculation.
                var_symbol = curr_symbol;
                next_token();
                //For function calls
                if(token == '('){
                    next_token();
                    while(token != ')'){
                        expression(Assign);
                        num_of_arguments++;
                        //Push each parameter in eax into stack
                        dbg_printf("PUSH EAX ;Push eax to stack.\n");
                        push_eax();
                        if(token == ','){
                            next_token();
                            continue;
                        }
                    }
                    match(')');
                    //Invoke function
                    dbg_printf("CALL %s[0x%llx]  ;function call.\n", var_symbol->name, var_symbol->addr.abs_addr);
                    call(var_symbol->addr.abs_addr);
                    //It is the caller's reponsibility to restore the stack
                    dbg_printf("ADD ESP, %lld\n",num_of_arguments*sizeof(long long));
                    add_esp_imm(num_of_arguments*sizeof(long long));

                    if(token > Assign){
                        //Push function call result to stack, i.e., push eax
                        dbg_printf("PUSH eax ; Put function [%s] return value to stack\n", var_symbol->name);
                        push_eax();
                        push_flag = true;
                    }
                    else if(token == Assign){
                        printf("Line %d, Illegal assignment to function call [%s]\n", line, var_symbol->name);
                        exit(-1);
                    }
                }
                //For variables
                else{
                    expr_type = var_symbol->data_type.var_type;
                    if(token > Assign){
                        //Push var into stack
                        dbg_printf("PUSH [%s] ;%lld\n", var_symbol->name, var_symbol->addr.abs_addr);
                        push_var(var_symbol);
                        push_flag = true;
                    }
                    //CORNER CASE: var_symbol is an pointer type, and the following operator is an assignment. (Ex. *p = expr.)
                    else if(token == Assign && is_var_pointer(var_symbol)){
                        //Push var into stack
                        dbg_printf("PUSH [%s] ;%lld\n", var_symbol->name, var_symbol->addr.abs_addr);
                        push_var(var_symbol);
                        push_flag = true;
                    }
                    //No operator behind id
                    else if(token == ';' || token == ',' || token == ')'){
                        //MOV eax, [id]
                        dbg_printf("MOV EAX, [%s] ;%lld\n", var_symbol->name, var_symbol->addr.abs_addr);
                        mov_var_to_eax(var_symbol); 
                        return true;
                    }
                }
            }
            else if(token == Num){
                imm_num = token_val;
                expr_type = Integer;
                next_token();
                if(token == ';' || token == ',' || token == ')'){
                    //MOV EAX, imm
                    dbg_printf("MOV EAX, %d\n",imm_num);
                    movimm_eax(imm_num);
                    return true;
                }
                //Push this number into stack
                dbg_printf("PUSH %d\n",imm_num);
                push_imm(imm_num);
                push_flag = true;               
            }
            else if(token == '('){
                next_token();
                expression(Assign);
                match(')');
                if(token == ';')
                    return true;
                //Push result in eax into stack for further calculation
                else if(token > Assign){
                    dbg_printf("PUSH EAX\n");
                    push_eax();
                    push_flag = true;
                }
                else if(token == Assign){
                    //CORNER CASE like '( id ) = expression'. As we don't have any better choice, we give var_symbol the most recent parsed symbol. 
                    if(is_var_pointer(curr_symbol) == false){
                        var_symbol = curr_symbol;
                    }
                    //For cases like ' *(id) = expr ', we do nothing here.
                }
            }
            else if(token == Mul){//Pointer dereference is a little peculiar.
                next_token();
                expression(Inc);
                expr_type -= Pointer;
                //The result of above evaluated expression should be an address. 
                if(token == Assign){//If there follows an assignment like {'*'}+ expression = expression
                    dbg_printf("MOV EBX, EAX\n"); //Save the addr in ebx.
                    mov_ebx_eax();
                    dbg_printf("MOV EAX, [EBX]\n");//Dereference ebx.
                    deref_eax_ebx();
                    pointer_deref_flag = true;
                }
                else{
                    dbg_printf("MOV EBX, EAX\n"); //Save the addr in ebx.
                    mov_ebx_eax();
                    dbg_printf("MOV EAX, [EBX]\n");//Dereference ebx.
                    deref_eax_ebx();
                    dbg_printf("PUSH EAX\n");
                    push_eax();
                    push_flag = true;
                }
            }
            else if(token == And){
                next_token();//For simplicity, suppose the next token is Id.
                if(token == Id){
                    if(curr_symbol->symbol_type == Glo){
                        dbg_printf("MOV EBX, absolute addr of %s: %llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                        movimm_ebx(curr_symbol->addr.abs_addr);
                    }
                    else if(curr_symbol->symbol_type == Loc){
                        dbg_printf("MOV EBX, relative addr of %s: %llx\n", curr_symbol->name, curr_symbol->addr.rel_stack_pos*sizeof(long long));
                        movimm_ebx(curr_symbol->addr.rel_stack_pos*sizeof(long long));
                        dbg_printf("ADD EBX, EBP\n");
                        add_ebx_ebp();
                    }
                }
                dbg_printf("PUSH EBX\n");
                push_ebx();
                push_flag = true;
                next_token();
            }
            else if(token == Sub){
                next_token();
                expression(Inc);
                dbg_printf("MOV EBX, 0\n");
                movimm_ebx(0);
                dbg_printf("SUB EBX, EAX\n");
                sub_ebx_eax();
                dbg_printf("PUSH EBX\n");
                push_ebx();
                push_flag = true;
            }
            else if(token == Add){
                next_token();
                expression(Inc);
                dbg_printf("PUSH EAX\n");
                push_eax();
                push_flag = true;
            }
            else if(token == Inc){
                next_token();
                //We support only expression like ++var; ignore the case ++(var) and the others. For pointer, it means "plus 8 (sizeof(i64))".
                if(token == Id && (curr_symbol->symbol_type == Loc || curr_symbol->symbol_type == Glo) ){
                    dbg_printf("MOV EBX, [%s] ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_var_to_ebx(curr_symbol);
                    if(is_var_pointer(curr_symbol)){
                        dbg_printf("ADD EBX, 8\n");
                        movimm_eax(8);
                        add_ebx_eax();
                    }
                    else{
                        dbg_printf("INC EBX\n");
                        inc_ebx();
                    }
                    dbg_printf("PUSH EBX \n");
                    push_ebx();
                    dbg_printf("MOV [%s], EBX ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_ebx_to_var(curr_symbol);
                    push_flag = true;
                }
                next_token();
            }
            else if(token == Dec){
                next_token();
                //We support only expression like --var; ignore the case --(var) and the others.TODO: For pointer, it means "plus 8".
                if(token == Id && (curr_symbol->symbol_type == Loc || curr_symbol->symbol_type == Glo) ){
                    dbg_printf("MOV EBX, [%s] ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_var_to_ebx(curr_symbol);
                    if(is_var_pointer(curr_symbol)){
                        dbg_printf("SUB EBX, 8\n");
                        movimm_eax(8);
                        sub_ebx_eax();
                    }
                    else{
                        dbg_printf("DEC EBX\n");
                        dec_ebx();
                    }
                    dbg_printf("PUSH EBX \n");
                    push_ebx();
                    dbg_printf("MOV [%s], EBX ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_ebx_to_var(curr_symbol);
                    push_flag = true;
                }
                next_token();
            }
            else if(token == Print){
                next_token();
                match('(');
                expression(Assign);
                match(')');
                dbg_printf("PRINT EAX\n");
                print_eax();
                return true;
            }
            else{
                return false;
            }

            while(token >= precedence){ //The legendary "precedence climbing"
                first_stack_elem_type = expr_type;
                if(token == Assign){                    
                    if(var_symbol != NULL){
                        next_token();
                        expression(Assign);
                        //Put the result in eax to var: MOV [id.addr], EAX
                        dbg_printf("MOVE [%s], EAX   ;Move result in eax to var %s[%lld].\n",var_symbol->name, var_symbol->name, var_symbol->addr.abs_addr);
                        mov_eax_to_var(var_symbol);
                        //It is not a good idea to create assignment statement like "( expression ) = expression" which tends to be murky and erroneous.
                    }
                    else{ //var_symbol == NULL, which implies the outer token should be *.
                        if(pointer_deref_flag == true){//If we reach here, then it will be a pointer assignment like {'*'}+ expression = expression or {'*'}+ var[expr] = expr. (Ex. *p = a; *p[0] = a;)
                            dbg_printf("PUSH EBX\n");//Here, the address to be dereferred is stored in ebx.
                            push_ebx();//Save this address in stack
                            next_token();
                            expression(Assign);//Evaluate the expression after '='.
                            dbg_printf("POP EBX\n");//Retrieve the address from stack
                            pop_ebx();
                            dbg_printf("MOV [EBX], EAX\n");
                            deref2_ebx_eax();
                        }
                    }
                    return true;
                }
                else if(token == Lor){
                    next_token();
                    expression(Land);
                    dbg_printf("Logical or: if either [first elem in stack] or eax is not zero, mov [first elem], 1; else mov [first elem], 0\n");
                    lor_stack_first_elem_eax();
                }
                else if(token == Land){
                    next_token();
                    expression(Or);
                    dbg_printf("Logical and: if either [first elem in stack] and eax is not zero, mov [first elem], 1; else mov [first elem], 0\n");
                    land_stack_first_elem_eax();
                }
                else if(token == Or){
                    next_token();
                    expression(Xor);
                    dbg_printf("OR [first elem in stack], EAX\n");
                    or_stack_first_elem_eax();
                }
                else if(token == Xor){
                    next_token();
                    expression(And);
                    dbg_printf("XOR [first elem in stack], EAX\n");
                    xor_stack_first_elem_eax();
                }
                else if(token == And){
                    next_token();
                    expression(Eq);
                    dbg_printf("AND [first elem in stack], EAX\n");
                    and_stack_first_elem_eax();
                }
                else if(token == Eq){
                    next_token();
                    expression(Lt);
                    dbg_printf("CMP [first elem in stack], EAX ; If equal, MOV [first], 1 else MOV [first],0 \n");
                    eq_stack_first_elem_eax();
                }
                else if(token == Ne){
                    next_token();
                    expression(Lt);
                    dbg_printf("CMP [first elem in stack], EAX ; If not equal, MOV [first], 1 else MOV [first],0 \n");
                    ne_stack_first_elem_eax();
                }
                else if(token == Lt){
                    next_token();
                    expression(Add);
                    dbg_printf("CMP [first elem in stack], EAX ; If it is less than eax, MOV [first], 1 else MOV [first],0 \n");
                    lt_stack_first_elem_eax();
                }
                else if(token == Gt){
                    next_token();
                    expression(Add);
                    dbg_printf("CMP [first elem in stack], EAX ; If it is greater than eax, MOV [first], 1 else MOV [first],0 \n");
                    gt_stack_first_elem_eax();
                }
                else if(token == Le){
                    next_token();
                    expression(Add);
                    dbg_printf("CMP [first elem in stack], EAX ; If it is less than or equal to eax, MOV [first], 1 else MOV [first],0 \n");
                    le_stack_first_elem_eax();
                }
                else if(token == Ge){
                    next_token();
                    expression(Add);
                    dbg_printf("CMP [first elem in stack], EAX ; If it is greater than or equal to eax, MOV [first], 1 else MOV [first],0 \n");
                    ge_stack_first_elem_eax();
                }
                else if(token == Add){
                    next_token();
                    expression(Mul);
                    if(first_stack_elem_type > Integer){
                        if(expr_type <= Integer){ //Pointer + Number
                            dbg_printf("MUL EAX, 8\n");
                            movimm_ebx(8);
                            mul_eax_ebx();
                            expr_type = first_stack_elem_type; // Implicit data type conversion: "Pointer + Number -> Pointer", so the expr_type should be set to the pointer type of first stack element.
                        }
                        else{ //Pointer + Pointer, which is meaningless. 
                            printf("Line %d: Addition between pointers is formidden.\n", line);
                            exit(-1);
                        }
                    }
                    else{
                        if(expr_type > Integer){ // Number + Pointer
                            dbg_printf("XCHG [first elem in stack], eax\n"); //A relatively complicated implementation as we don't have XCHG instruction for our virtual machine.
                            dbg_printf("MUL EAX, 8\n");
                            pop_ebx();
                            push_eax();
                            push_ebx();
                            movimm_eax(8);
                            mul_stack_first_elem_eax();
                            pop_eax();
                        }
                    }
                    dbg_printf("ADD [first elem in stack], EAX.\n");
                    add_stack_first_elem_eax();
                }
                else if(token == Sub){
                    next_token();
                    expression(Mul);
                    if(first_stack_elem_type > Integer){ //Pointer - Number
                        if(expr_type <= Integer){
                            dbg_printf("MUL EAX, 8\n");
                            movimm_ebx(8);
                            mul_eax_ebx();
                            expr_type = first_stack_elem_type;
                        }
                        else{ //Pointer - Pointer, where two pointers should have the same type.
                            if(expr_type != first_stack_elem_type){
                                printf("Line %d: Substraction between different pointer type is formidden.\n", line);
                                exit(-1);
                            }
                        }
                    }
                    else{
                        if(expr_type > Integer){ //Number - Pointer, which is meaningless.
                            printf("Line %d: Substracting a pointer from a number is formidden.\n", line);
                            exit(-1);
                        }
                    }
                    dbg_printf("SUB [first elem in stack], EAX.\n");
                    sub_stack_first_elem_eax();
                }
                else if(token == Mul){
                    next_token();
                    expression(Inc);
                    dbg_printf("MUL [first elem in stack], EAX.\n");
                    mul_stack_first_elem_eax();
                }
                else if(token == Div){
                    next_token();
                    expression(Inc);
                    dbg_printf("DIV [first elem in stack], EAX.\n");
                    div_stack_first_elem_eax();
                }
                else if(token == Mod){
                    next_token();
                    expression(Inc);
                    dbg_printf("MOD [first elem in stack], EAX.\n");
                    mod_stack_first_elem_eax();
                }
                else if(token == Inc){
                    dbg_printf("MOV EBX, [%s] ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_var_to_ebx(curr_symbol);
                    if(is_var_pointer(curr_symbol)){
                        dbg_printf("ADD EBX, 8\n");
                        movimm_eax(8);
                        add_ebx_eax();
                    }
                    else{
                        dbg_printf("INC EBX\n");
                        inc_ebx();
                    }
                    dbg_printf("MOV [%s], EBX ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_ebx_to_var(curr_symbol);
                    next_token();
                }
                else if(token == Dec){
                    dbg_printf("MOV EBX, [%s] ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_var_to_ebx(curr_symbol);
                    if(is_var_pointer(curr_symbol)){
                        dbg_printf("SUB EBX, 8\n");
                        movimm_eax(8);
                        sub_ebx_eax();
                    }
                    else{
                        dbg_printf("DEC EBX\n");
                        dec_ebx();
                    }
                    dbg_printf("MOV [%s], EBX ;%llx\n", curr_symbol->name, curr_symbol->addr.abs_addr);
                    mov_ebx_to_var(curr_symbol);
                    next_token();
                }
                else if(token == L_Sq_Bracket){
                    next_token();
                    expression(Assign);
                    match(']');
                    dbg_printf("MOV EBX, 8\n");
                    movimm_ebx(8);
                    dbg_printf("MUL EAX, EBX\n");
                    mul_eax_ebx();
                    dbg_printf("POP EBX\n");
                    pop_ebx();
                    dbg_printf("ADD EBX, EAX\n");
                    add_ebx_eax();
                    dbg_printf("MOV EAX, [EBX]\n");
                    deref_eax_ebx();
                    if(token != Assign){
                        dbg_printf("PUSH EAX\n");
                        push_eax();
                        push_flag = true;
                    }
                    else{//Ex. p[0] = expression;
                        var_symbol = NULL; //Force it to go through the same 'else' path in 'Assign' section, if the next token is assignment. 
                        push_flag = false;
                        pointer_deref_flag = true;
                    }
                }
            }

            if(push_flag == true){
                //Pop the first elem in stack back to eax as the result of expression, because We suppose the result is saved in eax for each expression.
                dbg_printf("POP EAX\n");
                pop_eax();
            }
            return true;
        }

public:
        void dump_generated_code()
        {
#ifdef __DEBUG__
            long long len = 0;
            long long *t = text - get_text_bytes()/sizeof(long long);
            long long i;
            char const *instruction_str[] = {
                NULL, "JMP", "CALL", "RET", "PUSH", "POP", "MOV", "MOVIMM", "LD", "ST", "LDSFO", "STSFO", "DEREF", "DEREF2",
                "OR", "XOR", "AND", "ADD", "SUB", "MUL", "DIV", "MOD", "INC", "DEC", 
                "CMP"/*Compare before any conditional jump*/, 
                "JA" /*if greater than*/, "JAE" /*if greater than or equal to*/, "JB" /*if less than*/, "JBE"/*if less than or equal to*/, "JZ", "JNZ", "JE", "JNE", 
                "EIP", "ESP", "EBP", "EAX", "EBX", /*Registers*/
                "EXIT", "PRINT"
            };
            printf("----Generated code dumped below.----");
            for(i = 0; i < get_text_bytes()/sizeof(long long); i++){
                if(!(len%16)){
                    printf("\n0x%llx:\t", (long long)t);
                }
                len++;
                if(*t>=1 && *t<=sizeof(instruction_str)/sizeof(instruction_str[0]))
                    printf("%s\t",instruction_str[*t++]);
                else
                    printf("%lld\t",*t++);
            }
            printf("\n\n");
#endif
        }
};

/*--- Virtual machine ---*/
#define PAGE_SIZE (1<<12)
#define STACK_SIZE (PAGE_SIZE)
#define CODE_SIZE (PAGE_SIZE)
#define DATA_SIZE (PAGE_SIZE)

#define i64 long long

#define IS_GREATER  (1<<2)
#define IS_EQUAL    (1<<1)
#define IS_LESS     (1<<0)

class tiny_vm{
protected:
    char *text, *data, *stack;
    i64 *eip, *esp, *ebp, eax, ebx, cflags;

public:
    tiny_vm(char *_text, char *_data, char *_stack) : text(_text), data(_data), stack(_stack){
        esp = ebp = (i64 *)(stack + STACK_SIZE); 
        eip = (i64 *)text;
    }

    void dump_reg()
    {
#ifdef __DEBUG__
        printf("eax = 0x%llx\n", eax);
        printf("ebx = 0x%llx\n", ebx);
        printf("esp = 0x%llx\n", (i64)esp);
        printf("ebp = 0x%llx\n", (i64)ebp);
        printf("eip = 0x%llx\n", (i64)eip);
        printf("cflags = 0x%llx\n", cflags);
#endif
    }

    void err_msg_exit(const char *instruction)
    {
        printf("Error in parsing %s\n", instruction);
        dump_reg();
        exit(-1);
    }

    void create_cflags(i64 reg1, i64 reg2)
    {
        i64 res = reg1 - reg2;
        if(res > 0)
            cflags = IS_GREATER;
        else if(res == 0)
            cflags = IS_EQUAL;
        else
            cflags = IS_LESS;
    }

    int run()
    {
        i64 op = *eip++;
        i64 operand1, operand2;

    /*--To simplify the implementation of virtual machine, only the following instructions are supported. 
        MOV reg1, reg2
        MOVIMM reg, imm ;reg = EAX | EBX
        LD reg, [Addr] ;reg = EAX | EBX
        ST [Addr], reg ;reg = EAX | EBX
        LDSFO reg, [Offset]; reg = EAX | EBX
        STSFO [Offset], reg; reg = EAX | EBX
        PUSH reg           ; reg = EAX | EBX | EBP
        POP reg            ; reg = EAX | EBX | EBP
    --*/
        while(1){
            if(op == MOV){
                /*MOV reg, reg*/
                operand1 = *eip++;
                operand2 = *eip++;            
                //MOV EBP, ESP
                if(operand1 == EBP && operand2 == ESP){
                    ebp = esp;
                }
                //MOV ESP, EBP
                else if(operand1 == ESP && operand2 == EBP){
                    esp = ebp;
                }
                //MOV ESP, EAX
                else if(operand1 == ESP && operand2 == EAX){
                    esp = (i64 *)eax;
                }
                //MOV EAX, ESP
                else if(operand1 == EAX && operand2 == ESP){
                    eax = (i64)esp;
                }
                //MOV ESP, EBX
                else if(operand1 == ESP && operand2 == EBX){
                    esp = (i64 *)ebx;
                }
                //MOV EBX, ESP
                else if(operand1 == EBX && operand2 == ESP){
                    ebx = (i64)esp;
                }
                //MOV EIP, EAX
                else if(operand1 == EIP && operand2 == EAX){
                    eip = (i64 *)eax;
                }
                //MOV EAX, EIP
                else if(operand1 == EAX && operand2 == EIP){
                    eax = (i64)eip;
                }
                //MOV EIP, EBX
                else if(operand1 == EIP && operand2 == EBX){
                    eip = (i64 *)ebx;
                }
                //MOV EBX, EIP
                else if(operand1 == EBX && operand2 == EIP){
                    ebx = (i64)eip;
                }
                //MOV EAX, EBX
                else if(operand1 == EAX && operand2 == EBX){
                    eax = ebx;
                }
                //MOV EBX, EAX
                else if(operand1 == EBX && operand2 == EAX){
                    ebx = eax;
                }
                else{
                    err_msg_exit("MOV");
                }
            }
            else if(op == MOVIMM){
                operand1 = *eip++;
                operand2 = *eip++; 
                //MOV EAX, imm
                if(operand1 == EAX){
                    eax = operand2;
                }
                //MOV EBX, imm
                else if(operand1 == EBX){
                    ebx = operand2;
                }
                else{
                    err_msg_exit("MOVIMM");
                }
            }
            else if(op == LD){
                operand1 = *eip++;
                operand2 = *eip++; 
                //LD EAX, [Addr] 
                if(operand1 == EAX){
                    eax = *(i64 *)operand2;
                }
                //LD EBX, [Addr]
                else if(operand1 == EBX){  
                    ebx = *(i64 *)operand2;                              
                }
                else{
                    err_msg_exit("LD");
                }
            }
            else if (op == ST){
                operand1 = *eip++;
                operand2 = *eip++;
                //ST [Addr], EAX
                if(operand2 == EAX){
                    *(i64 *)operand1 = eax;
                }
                //ST [Addr], EBX
                else if(operand2 == EBX){
                    *(i64 *)operand1 = ebx;
                }
                else{
                    err_msg_exit("ST");
                }
            }
            else if(op == LDSFO){
                operand1 = *eip++;
                operand2 = *eip++;
                //LDSFO EAX, [Offset]
                if(operand1 == EAX){
                    eax = ebp[operand2];
                }
                //LDSFO EBX, [Offset]
                else if(operand1 == EBX){
                    ebx = ebp[operand2];
                }
                else{
                    err_msg_exit("LDSFO");
                }
            }
            else if(op == STSFO){
                operand1 = *eip++;
                operand2 = *eip++;
                //STSFO [Offset], EAX
                if(operand2 == EAX){
                    ebp[operand1] = eax;
                }
                //STSFO [Offset], EBX
                else if(operand2 == EBX){
                    ebp[operand1] = ebx;
                }
                else{
                    err_msg_exit("STSFO");
                }
            }
            else if(op == DEREF){
                operand1 = *eip++;
                operand2 = *eip++;
                //DEREF EAX, EBX -> MOV EAX, [EBX]
                if(operand1 == EAX && operand2 == EBX){
                    eax = *(i64 *)ebx;
                }
                //DEREF EBX, EAX -> MOV EBX, [EAX]
                else if(operand1 == EBX && operand2 == EAX){
                    ebx = *(i64 *)eax;
                }
            }
            else if(op == DEREF2){
                operand1 = *eip++;
                operand2 = *eip++;
                //DEREF2 EAX, EBX -> MOV [EAX], EBX
                if(operand1 == EAX && operand2 == EBX){
                    *(i64 *)eax = ebx;
                }
                //DEREF2 EBX, EAX -> MOV [EBX], EAX
                else if(operand1 == EBX && operand2 == EAX){
                    *(i64 *)ebx = eax;
                }
            }
            else if(op == PUSH){
                operand1 = *eip++;
                //PUSH EAX
                if(operand1 == EAX){
                    esp -= 1;
                    *esp = eax;
                }
                //PUSH EBX
                else if(operand1 == EBX){
                    esp -= 1;
                    *esp = ebx;
                }
                //PUSH EBP
                else if(operand1 == EBP){
                    esp -= 1;
                    *esp = (i64)ebp;
                }
                else{
                    err_msg_exit("PUSH");
                }
            }
            else if(op == POP){
                operand1 = *eip++;
                //POP EAX
                if(operand1 == EAX){
                    eax = *esp;
                    esp += 1;
                }
                //POP EBX
                else if(operand1 == EBX){
                    ebx = *esp;
                    esp += 1;
                }
                //POP EBP
                else if(operand2 == EBP){
                    ebp = (i64 *)*esp;
                    esp += 1;
                }
                else{
                    err_msg_exit("POP");
                }
            }
            else if(op == JMP){
                //JMP [Addr]
                eip = (i64 *)*eip;
            }
            else if(op == CALL){
                //CALL [Addr]
                *--esp = (i64)(eip+1);
                eip = (i64 *)*eip;
            }
            else if(op == RET){
                //RET
                eip = (i64 *)*esp++;
            }
            else if (op == ADD){
                operand1 = *eip++;
                operand2 = *eip++;
                //ADD eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax += ebx;
                }
                //ADD ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx += eax;
                }
                //ADD esp, eax
                else if(operand1 == ESP && operand2 == EAX){
                    esp = (i64 *)((char *)esp + eax);
                }
                //ADD esp, ebx
                else if(operand1 == ESP && operand2 == EBX){
                    esp = (i64 *)((char *)esp + ebx);
                }
                //ADD ebx, ebp
                else if(operand1 == EBX && operand2 == EBP){
                    ebx = ebx + (i64)ebp;
                }
                else{
                    err_msg_exit("ADD");
                }
            }
            else if (op == SUB){
                operand1 = *eip++;
                operand2 = *eip++;
                //SUB eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax -= ebx;
                }
                //SUB ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx -= eax;
                }
                //SUB esp, eax
                else if(operand1 == ESP && operand2 == EAX){
                    esp = (i64 *)((char *)esp - eax);
                }
                //SUB esp, ebx
                else if(operand1 == ESP && operand2 == EBX){
                    esp = (i64 *)((char *)esp - ebx);
                }
                else{
                    err_msg_exit("SUB");
                }
            }
            else if(op == MUL){
                operand1 = *eip++;
                operand2 = *eip++;
                //MUL eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax *= ebx;
                }
                //MUL ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx *= eax;
                }
                else{
                    err_msg_exit("MUL");
                }
            }
            else if(op == DIV){
                operand1 = *eip++;
                operand2 = *eip++;
                //DIV eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax /= ebx;
                }
                //DIV ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx /= eax;
                }
                else{
                    err_msg_exit("DIV");
                }
            }
            else if(op == AND){
                operand1 = *eip++;
                operand2 = *eip++;
                //AND eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax &= ebx;
                }
                //AND ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx &= eax;
                }
                else{
                    err_msg_exit("AND");
                }
            }
            else if(op == OR){
                operand1 = *eip++;
                operand2 = *eip++;
                //OR eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax |= ebx;
                }
                //OR ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx |= eax;
                }
                else{
                    err_msg_exit("OR");
                }
            }
            else if(op == XOR){
                operand1 = *eip++;
                operand2 = *eip++;
                //XOR eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax ^= ebx;
                }
                //XOR ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx ^= eax;
                }
                else if(operand1 == EAX && operand2 == EAX){
                    eax ^= eax;
                }
                else if(operand1 == EBX && operand2 == EBX){
                    ebx ^= ebx;
                }
                else{
                    err_msg_exit("XOR");
                }
            }
            else if(op == MOD){
                operand1 = *eip++;
                operand2 = *eip++;
                //MOD eax, ebx
                if(operand1 == EAX && operand2 == EBX){
                    eax %= ebx;
                }
                //MOD ebx, eax
                else if(operand1 == EBX && operand2 == EAX){
                    ebx %= eax;
                }
                else{
                    err_msg_exit("MOD");
                }
            }
            else if(op == INC){
                operand1 = *eip++;
                //INC EAX
                if(operand1 == EAX){
                    ++eax;
                }
                //INC EBX
                else if(operand1 == EBX){
                    ++ebx;
                }
            }
            else if(op == DEC){
                operand1 = *eip++;
                //DEC EAX
                if(operand1 == EAX){
                    --eax;
                }
                //DEC EBX
                else if(operand1 == EBX){
                    --ebx;
                }
            }
            else if (op == CMP){
                operand1 = *eip++;
                operand2 = *eip++;
                //CMP EAX, EBX
                if(operand1 == EAX && operand2 == EBX){
                    create_cflags(eax, ebx);
                }
                //CMP EBX, EAX
                else if(operand1 == EBX && operand2 == EAX){
                    create_cflags(ebx, eax);
                }
                else{
                    err_msg_exit("CMP");
                }
            }
            else if (op == JZ || op == JE){
                //JZ [Addr]
                operand1 = *eip++;
                if(cflags & IS_EQUAL){
                    eip = (i64 *)operand1;
                    }
            }
            else if (op == JNZ || op == JNE){
                //JNZ [Addr]
                operand1 = *eip++;
                if(!(cflags & IS_EQUAL)){
                    eip = (i64 *)operand1;
                }
            }
            else if (op == JA){
                //JA [Addr] ; >
                operand1 = *eip++;
                if(cflags & IS_GREATER){
                    eip = (i64 *)operand1;
                }
            }
            else if (op == JAE){
                //JAE [Addr] ; >=
                operand1 = *eip++;
                if(cflags & IS_GREATER || cflags & IS_EQUAL){
                    eip = (i64 *)operand1;
                }
            }
            else if (op == JB){
                //JB [Addr] ; <
                operand1 = *eip++;
                if(cflags & IS_LESS){
                    eip = (i64 *)operand1;
                }
            }
            else if (op == JBE){
                operand1 = *eip++;
                if(cflags & IS_LESS || cflags & IS_EQUAL){
                    eip = (i64 *)operand1;
                }
            }
            else if (op == EXIT){
                printf("EXIT\n");
                return 0;
            }
            else if (op == PRINT){
                operand1 == *eip++;
                //PRINT EAX
                if(operand1 == EAX){
                    printf("%lld\n", eax);
                }
                //PRINT EBX
                else if(operand1 == EBX){
                    printf("%lld\n", ebx);
                }
            }
            else{
                printf("op == %lld\n", op);
                err_msg_exit("Unkown instruction\n");
            }

            op = *eip++;
        }
        return -1;
    }

};

int main()
{
    //char src[] = "int a; int b, c, _de; \n int add(int a, int b) \n {int c; c = a + b;return c;} \n int main(int argc, int argv) \n {int b,c,res; a=3; res = add(a=1-(a-5),4)+add(3,a*(3%5)); if(res>20 || res<=18) return (res&255); else {while(res < 100){res = res + 1;} printf(++res*(3-4)); return res---a++;}}";
    //char src[] = "int fib(int n){if(n<=0) return 0; else if(n<=2) return 1; return fib(n-1)+fib(n-2);} int main(){int res; res = fib(20); printf(res); return 0;}";
    char src[] = "int ab; int main() {int a,b; int **pp; int *p, *p2, ***ppp; a=10; p=&a; p2=&b; pp = &p; ppp = &pp; printf(pp);  a=b = 20; **pp++ = + 30+*p*5/(a-10); pp--;printf(*pp[0]); **ppp[0] = 55; printf(p[0]);  *p-- = 33; printf(**pp);(b)=**pp/11-3*4;printf(*p2);*p++=2;printf(p);printf(b+*p/11+p-1); *(p-1) = b+4;printf(b);}";
    /*class tokenizer tokenizer(src, 4096);
    tokenizer.run();
    tokenizer.iterate_global_symbols();*/
    char text[CODE_SIZE], data[DATA_SIZE], stack[STACK_SIZE];
    class parser parser(text, data, src);
    parser.run();
    parser.iterate_global_symbols();//Need #define __DEBUG__
    parser.dump_generated_code();//Need #define __DEBUG__

    class tiny_vm tiny_vm(text, data, stack); 
    tiny_vm.run();
    tiny_vm.dump_reg();//Need #define __DEBUG__

    return 0;
}