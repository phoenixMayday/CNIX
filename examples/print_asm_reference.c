/*
 * CNIX Print Statement — ASM Generation Reference
 * =================================================
 *
 * This file shows how gen_stmt would handle a hypothetical print statement
 * for the CNIX compiler, following the existing code‑generation conventions
 * in generator.c (x86‑64 Linux, AT&T syntax, stack‑based evaluation).
 *
 * Linux write syscall:
 *   rax = 1          (syscall number for sys_write)
 *   rdi = fd         (1 = stdout, 2 = stderr)
 *   rsi = buf        (pointer to the data)
 *   rdx = count      (number of bytes to write)
 *   syscall           → returns bytes written in rax
 *
 * ---------------------------------------------------------------------------
 * CASE 1 — Print a single character (value on the stack)
 * ---------------------------------------------------------------------------
 *
 * Source:   print('A');          // or: print(myCharVar);
 * Strategy: evaluate expression → push to stack → use stack address as buf
 *
 *   // Step 1: evaluate the expression (gen_expr pushes a qword)
 *   gen_expr(stmt->as.stmt_print->expr, ctx);
 *
 *   // Step 2: emit the write(1, rsp, 1) syscall using the top of stack as buffer
 *   appendf(&ctx->output,
 *       "    # print: write single byte to stdout\n"
 *       "    movq $1, %%rax\n"           // sys_write
 *       "    movq $1, %%rdi\n"           // fd = stdout
 *       "    movq %%rsp, %%rsi\n"        // buf = top of stack (the pushed value)
 *       "    movq $1, %%rdx\n"           // count = 1 byte
 *       "    syscall\n"
 *       "    addq $8, %%rsp\n");         // pop the qword we evaluated
 *
 *   ctx->stack_size -= QWORD;
 *
 *
 * Generated assembly (for print('A')  ──  'A' == 65):
 *
 *       # push character literal `65`
 *       movq $65, %rax
 *       pushq %rax
 *       # print: write single byte to stdout
 *       movq $1, %rax            # sys_write
 *       movq $1, %rdi            # fd = stdout
 *       movq %rsp, %rsi          # buf = top of stack
 *       movq $1, %rdx            # count = 1
 *       syscall
 *       addq $8, %rsp            # clean up the pushed value
 *
 *
 * ---------------------------------------------------------------------------
 * CASE 2 — Print a stack‑allocated string (char array variable)
 * ---------------------------------------------------------------------------
 *
 * Source:   char[] msg = "hello";
 *           print(msg, 5);       // print(buffer_expr, length_expr)
 *
 * Strategy: push the array address (leaq), push the length, then syscall.
 *
 *   // Step 1: generate length expression
 *   gen_expr(stmt->as.stmt_print->length, ctx);
 *
 *   // Step 2: generate buffer expression (for an ident that is a char[],
 *   //         gen_term already pushes the leaq address)
 *   gen_expr(stmt->as.stmt_print->expr, ctx);
 *
 *   // Step 3: emit the write syscall
 *   appendf(&ctx->output,
 *       "    # print: write buffer to stdout\n"
 *       "    popq %%rsi\n"               // buf   (address of the string)
 *       "    popq %%rdx\n"               // count (length)
 *       "    movq $1, %%rax\n"           // sys_write
 *       "    movq $1, %%rdi\n"           // fd = stdout
 *       "    syscall\n");
 *
 *   ctx->stack_size -= 2 * QWORD;       // popped both values
 *
 *
 * Generated assembly (for the "hello" example):
 *
 *       # push character literal `111`   ('o' — pushed first, arrays store in reverse)
 *       movq $111, %rax
 *       pushq %rax
 *       # adjust stack pointer to shrink array element to byte
 *       addq $3, %rsp
 *         ...                            (repeats for 'l', 'l', 'e', 'h')
 *       # (variable msg is now on the stack as 5 contiguous bytes)
 *
 *       # push integer literal `5`       (length)
 *       movq $5, %rax
 *       pushq %rax
 *
 *       # load variable `msg` (array address)
 *       leaq <offset>(%rsp), %rax
 *       pushq %rax
 *
 *       # print: write buffer to stdout
 *       popq %rsi                        # buf   = address of "hello"
 *       popq %rdx                        # count = 5
 *       movq $1, %rax                    # sys_write
 *       movq $1, %rdi                    # fd = stdout
 *       syscall
 *
 *
 * ---------------------------------------------------------------------------
 * CASE 3 — Print a heap‑allocated buffer (pointer variable)
 * ---------------------------------------------------------------------------
 *
 * Source:   qword* buf = alloc(64);
 *           buf[0] = 'H';   buf[1] = 'i';
 *           print(buf, 2);
 *
 * Same syscall pattern as Case 2 — gen_term will push the pointer *value*
 * (movq instead of leaq), and the rest is identical.
 *
 *
 * ---------------------------------------------------------------------------
 * CASE 4 — Print with a newline
 * ---------------------------------------------------------------------------
 *
 * If you want a println that appends '\n', you can:
 *   (a) push 10 ('\n') onto the stack after the main write and do a
 *       second 1‑byte write, or
 *   (b) store the newline in a .data / .rodata section label and write it.
 *
 * Quick approach (a) — two syscalls:
 *
 *   // ... first do the normal print (Case 1 or 2) ...
 *
 *   appendf(&ctx->output,
 *       "    # println: append newline\n"
 *       "    pushq $10\n"                // '\n' == 10
 *       "    movq $1, %%rax\n"           // sys_write
 *       "    movq $1, %%rdi\n"           // stdout
 *       "    movq %%rsp, %%rsi\n"        // buf = stack top
 *       "    movq $1, %%rdx\n"           // count = 1
 *       "    syscall\n"
 *       "    addq $8, %%rsp\n");         // pop the newline
 *
 *   // stack_size stays unchanged (push then pop)
 *
 *
 * ---------------------------------------------------------------------------
 * INTEGRATION SKETCH — where this fits in gen_stmt
 * ---------------------------------------------------------------------------
 *
 *   // In gen_stmt, after the existing `else if` chain:
 *
 *   else if (stmt->kind == NODE_STMT_PRINT) {
 *       // single‑char print
 *       gen_expr(stmt->as.stmt_print->expr, ctx);
 *
 *       appendf(&ctx->output,
 *           "    # print: write single byte to stdout\n"
 *           "    movq $1, %%rax\n"
 *           "    movq $1, %%rdi\n"
 *           "    movq %%rsp, %%rsi\n"
 *           "    movq $1, %%rdx\n"
 *           "    syscall\n"
 *           "    addq $8, %%rsp\n");
 *
 *       ctx->stack_size -= QWORD;
 *   }
 *
 *   // Or for a buffer + length variant:
 *
 *   else if (stmt->kind == NODE_STMT_PRINT_BUF) {
 *       gen_expr(stmt->as.stmt_print_buf->length, ctx);
 *       gen_expr(stmt->as.stmt_print_buf->expr, ctx);
 *
 *       appendf(&ctx->output,
 *           "    # print: write buffer to stdout\n"
 *           "    popq %%rsi\n"           // buf
 *           "    popq %%rdx\n"           // count
 *           "    movq $1, %%rax\n"       // sys_write
 *           "    movq $1, %%rdi\n"       // fd = stdout
 *           "    syscall\n");
 *
 *       ctx->stack_size -= 2 * QWORD;
 *   }
 *
 * ---------------------------------------------------------------------------
 * NOTES
 * ---------------------------------------------------------------------------
 *
 * - The write syscall clobbers rcx and r11 (all syscalls do). None of the
 *   existing codegen relies on those registers being preserved, so this is
 *   fine with the current approach.
 *
 * - `appendf` uses the `%%` escape for literal `%` in AT&T register names
 *   because it runs through printf internally.
 *
 * - Stack alignment: the existing codegen doesn't enforce 16‑byte alignment
 *   (pushq always keeps 8‑byte alignment), and syscalls don't require it,
 *   so no extra padding is needed.
 *
 * - To also support printing integers as decimal strings you'd need an
 *   itoa‑style conversion routine (either inline asm with a loop, or a
 *   helper function emitted once and called). That's a larger addition
 *   beyond this reference.
 */
