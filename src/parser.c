#include "lexer.c"

typedef struct {
    Token *tokens;
    int current_pos;
    int token_count;
} ParserCtx;

// Terms
typedef enum {
    NODE_TERM_INT_LIT,
    NODE_TERM_IDENT
} NodeTermKind;

typedef struct {
    Token int_lit;
} NodeTermIntLit;

typedef struct {
    Token ident;
} NodeTermIdent;

typedef struct {
    NodeTermKind kind;
    union {
        NodeTermIntLit *int_lit;
        NodeTermIdent *ident;
    } as;
} NodeTerm;

// Expressions
typedef enum {
    NODE_EXPR_ADD,
    NODE_EXPR_SUB,
    NODE_EXPR_MUL,
    NODE_EXPR_DIV,
    NODE_EXPR_TERM
} NodeExprKind;

typedef struct NodeExpr NodeExpr;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprAdd;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprSub;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprMul;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprDiv;

typedef struct NodeExpr{
    NodeExprKind kind;
    union {
        NodeExprAdd *add;
        NodeExprSub *sub;
        NodeExprMul *mul;
        NodeExprDiv *div;
        NodeTerm *term;
    } as;
} NodeExpr;

// Statements
typedef enum {
    NODE_STMT_EXIT,
    NODE_STMT_VAR
} NodeStmtKind;

typedef struct {
    NodeExpr *expr;
} NodeStmtExit;

typedef struct {
    Token ident;
    NodeExpr *expr;
} NodeStmtVar;

typedef struct {
    NodeStmtKind kind;
    union {
        NodeStmtExit *stmt_exit;
        NodeStmtVar *stmt_var;
    } as;
} NodeStmt;

typedef struct {
    NodeStmt **stmts;
    int stmt_count;
} NodeProg;


NodeTerm *parse_term(ParserCtx *ctx) {
    Token *peek = &ctx->tokens[ctx->current_pos];
    if (peek->type == TOKEN_INT_LIT) {
        NodeTermIntLit *int_lit_node = malloc(sizeof(NodeTermIntLit));
        int_lit_node->int_lit = *peek;

        NodeTerm *term_node = malloc(sizeof(NodeTerm));
        term_node->kind = NODE_TERM_INT_LIT;
        term_node->as.int_lit = int_lit_node;

        ctx->current_pos++;
        return term_node;
    } else if (peek->type == TOKEN_IDENT) {
        NodeTermIdent *ident_node = malloc(sizeof(NodeTermIdent));
        ident_node->ident = *peek;
        
        NodeTerm *term_node = malloc(sizeof(NodeTerm));
        term_node->kind = NODE_TERM_IDENT;
        term_node->as.ident = ident_node;

        ctx->current_pos++;
        return term_node;
    }
    else {
        fprintf(stderr, "Unexpected token in term\n");
        exit(EXIT_FAILURE);
    }
}

NodeExpr *parse_expr(ParserCtx *ctx) {
    // eval left term
    NodeTerm *lhs_term = parse_term(ctx);
    NodeExpr *lhs_expr = malloc(sizeof(NodeExpr));
    lhs_expr->kind = NODE_EXPR_TERM;
    lhs_expr->as.term = lhs_term;
    
    // end of token stream
    if (ctx->current_pos >= ctx->token_count) {
        return lhs_expr;
    }

    Token *peek = &ctx->tokens[ctx->current_pos];
    if (peek->type != TOKEN_PLUS &&
        peek->type != TOKEN_MINUS &&
        peek->type != TOKEN_MUL &&
        peek->type != TOKEN_DIV) {
        return lhs_expr;
    }

    // eval operator
    Token *op = &ctx->tokens[ctx->current_pos];
    ctx->current_pos++;

    NodeExpr *result_expr = malloc(sizeof(NodeExpr));
    if (op->type == TOKEN_PLUS) {
        NodeExprAdd *add_node = malloc(sizeof(NodeExprAdd));
        add_node->lhs = lhs_expr;
        add_node->rhs = parse_expr(ctx);
        result_expr->kind = NODE_EXPR_ADD;
        result_expr->as.add = add_node;
    } else if (op->type == TOKEN_MINUS) {
        NodeExprSub *sub_node = malloc(sizeof(NodeExprSub));
        sub_node->lhs = lhs_expr;
        sub_node->rhs = parse_expr(ctx);
        result_expr->kind = NODE_EXPR_SUB;
        result_expr->as.sub = sub_node;
    } else if (op->type == TOKEN_MUL) {
        NodeExprMul *mul_node = malloc(sizeof(NodeExprMul));
        mul_node->lhs = lhs_expr;
        mul_node->rhs = parse_expr(ctx);
        result_expr->kind = NODE_EXPR_MUL;
        result_expr->as.mul = mul_node;
    } else if (op->type == TOKEN_DIV) {
        NodeExprDiv *div_node = malloc(sizeof(NodeExprDiv));
        div_node->lhs = lhs_expr;
        div_node->rhs = parse_expr(ctx);
        result_expr->kind = NODE_EXPR_DIV;
        result_expr->as.div = div_node;
    } else {
        fprintf(stderr, "Unexpected operator in expression: %d\n", op->type);
        exit(EXIT_FAILURE);
    }

    return result_expr;
}

NodeStmt *parse_stmt(ParserCtx *ctx) {
    Token *current = &ctx->tokens[ctx->current_pos];
    if (current->type == TOKEN_EXIT) {
        ctx->current_pos++;

        // expect expression
        NodeExpr *expr = parse_expr(ctx);

        // expect semicolon
        if (ctx->current_pos >= ctx->token_count || ctx->tokens[ctx->current_pos].type != TOKEN_SEMI) {
            fprintf(stderr, "Expected ';' at end of statement\n");
            exit(EXIT_FAILURE);
        }
        ctx->current_pos++;

        NodeStmtExit *stmt_exit = malloc(sizeof(NodeStmtExit));
        stmt_exit->expr = expr;

        NodeStmt *stmt_node = malloc(sizeof(NodeStmt));
        stmt_node->kind = NODE_STMT_EXIT;
        stmt_node->as.stmt_exit = stmt_exit;

        return stmt_node;
    } else if (current->type == TOKEN_VAR) {
        ctx->current_pos++;
        Token ident_token = ctx->tokens[ctx->current_pos];

        // expect identifier
        if (ctx->current_pos >= ctx->token_count || ctx->tokens[ctx->current_pos].type != TOKEN_IDENT) {
            fprintf(stderr, "Expected identifier after 'var'\n");
            exit(EXIT_FAILURE);
        }
        ctx->current_pos++;

        // expect equals
        if (ctx->current_pos >= ctx->token_count || ctx->tokens[ctx->current_pos].type != TOKEN_EQUALS) {
            fprintf(stderr, "Expected '=' after variable name\n");
            exit(EXIT_FAILURE);
        }
        ctx->current_pos++;

        // expect expression
        NodeExpr *expr = parse_expr(ctx);

        // expect semicolon
        if (ctx->current_pos >= ctx->token_count || ctx->tokens[ctx->current_pos].type != TOKEN_SEMI) {
            fprintf(stderr, "Expected ';' at end of statement\n");
            exit(EXIT_FAILURE);
        }
        ctx->current_pos++;

        NodeStmtVar *stmt_var = malloc(sizeof(NodeStmtVar));
        stmt_var->ident = ident_token;
        stmt_var->expr = expr;

        NodeStmt *stmt_node = malloc(sizeof(NodeStmt));
        stmt_node->kind = NODE_STMT_VAR;
        stmt_node->as.stmt_var = stmt_var;

        return stmt_node;
    }
    else {
        fprintf(stderr, "Unexpected token in statement\n");
        exit(EXIT_FAILURE);
    }
}

NodeProg *parse_prog(Token **tokens, int token_count) {
    NodeProg *prog = malloc(sizeof(NodeProg));
    prog->stmts = NULL;
    prog->stmt_count = 0;

    ParserCtx ctx = {
        .tokens = *tokens,
        .current_pos = 0,
        .token_count = token_count
    };

    while (ctx.current_pos < token_count) {
        NodeStmt *stmt = parse_stmt(&ctx);
        prog->stmts = realloc(prog->stmts, sizeof(NodeStmt*) * (prog->stmt_count + 1));
        prog->stmts[prog->stmt_count] = stmt;
        prog->stmt_count++;
    }

    return prog;
}