#include "lexer.c"

typedef struct {
    Token *tokens;
    int current_pos;
    int token_count;
} ParserCtx;

void expect_token(ParserCtx *ctx, TokenType expected_type) {
    if (ctx->current_pos >= ctx->token_count || ctx->tokens[ctx->current_pos].type != expected_type) {
        fprintf(stderr, "Expected token type %d\n", expected_type);
        exit(EXIT_FAILURE);
    }
    ctx->current_pos++;
}

// Forward declarations
typedef struct NodeExpr NodeExpr;
typedef struct NodeStmt NodeStmt;
NodeExpr *parse_expr(ParserCtx *ctx, int min_prec);

// Terms
typedef enum {
    NODE_TERM_INT_LIT,
    NODE_TERM_IDENT,
    NODE_TERM_PAREN
} NodeTermKind;

typedef struct {
    Token int_lit;
} NodeTermIntLit;

typedef struct {
    Token ident;
} NodeTermIdent;

typedef struct{
    NodeExpr *expr;
} NodeTermParen;

typedef struct {
    NodeTermKind kind;
    union {
        NodeTermIntLit *int_lit;
        NodeTermIdent *ident;
        NodeTermParen *paren;
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

typedef struct NodeExpr {
    NodeExprKind kind;
    union {
        NodeExprAdd *add;
        NodeExprSub *sub;
        NodeExprMul *mul;
        NodeExprDiv *div;
        NodeTerm *term;
    } as;
} NodeExpr;

// Scope
typedef struct {
    NodeStmt **stmts;
    int stmt_count;
} NodeScope;

// Statements
typedef enum {
    NODE_STMT_EXIT,
    NODE_STMT_VAR,
    NODE_STMT_SCOPE,
    NODE_STMT_IF
} NodeStmtKind;

typedef struct {
    NodeExpr *expr;
} NodeStmtExit;

typedef struct {
    Token ident;
    NodeExpr *expr;
} NodeStmtVar;

typedef struct {
    NodeScope *scope;
} NodeStmtScope;

typedef struct {
    NodeExpr *expr;
    NodeScope *scope;
} NodeStmtIf;

typedef struct NodeStmt {
    NodeStmtKind kind;
    union {
        NodeStmtExit *stmt_exit;
        NodeStmtVar *stmt_var;
        NodeStmtScope *stmt_scope;
        NodeStmtIf *stmt_if;
    } as;
} NodeStmt;

// Program
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
    } else if (peek->type == TOKEN_OPEN_PAREN) {
        ctx->current_pos++; // consume '('

        // expect expression
        NodeTermParen *paren_node = malloc(sizeof(NodeTermParen));
        paren_node->expr = parse_expr(ctx, 0);

        expect_token(ctx, TOKEN_CLOSE_PAREN); 

        NodeTerm *term_node = malloc(sizeof(NodeTerm));
        term_node->kind = NODE_TERM_PAREN;
        term_node->as.paren = paren_node;

        return term_node;
    }
    else {
        fprintf(stderr, "Unexpected token in term\n");
        exit(EXIT_FAILURE);
    }
}

NodeExpr *parse_expr(ParserCtx *ctx, int min_prec) {
    // eval left term
    NodeTerm *lhs_term = parse_term(ctx);
    NodeExpr *lhs_expr = malloc(sizeof(NodeExpr));
    lhs_expr->kind = NODE_EXPR_TERM;
    lhs_expr->as.term = lhs_term;
    
    while (ctx->current_pos < ctx->token_count) {
        Token *peek = &ctx->tokens[ctx->current_pos];
        int prec = get_precedence(peek->type);
        if (prec == 0 || prec < min_prec) break;

        // eval operator
        Token op = *peek;
        ctx->current_pos++;

        // parse right-hand side expression with higher precedence for left associativity
        NodeExpr *rhs_expr = parse_expr(ctx, prec + 1);

        NodeExpr *combined = malloc(sizeof(NodeExpr));
        if (op.type == TOKEN_PLUS) {
            NodeExprAdd *add_node = malloc(sizeof(NodeExprAdd));
            add_node->lhs = lhs_expr;
            add_node->rhs = rhs_expr;
            combined->kind = NODE_EXPR_ADD;
            combined->as.add = add_node;
        } else if (op.type == TOKEN_MINUS) {
            NodeExprSub *sub_node = malloc(sizeof(NodeExprSub));
            sub_node->lhs = lhs_expr;
            sub_node->rhs = rhs_expr;
            combined->kind = NODE_EXPR_SUB;
            combined->as.sub = sub_node;
        } else if (op.type == TOKEN_MUL) {
            NodeExprMul *mul_node = malloc(sizeof(NodeExprMul));
            mul_node->lhs = lhs_expr;
            mul_node->rhs = rhs_expr;
            combined->kind = NODE_EXPR_MUL;
            combined->as.mul = mul_node;
        } else if (op.type == TOKEN_DIV) {
            NodeExprDiv *div_node = malloc(sizeof(NodeExprDiv));
            div_node->lhs = lhs_expr;
            div_node->rhs = rhs_expr;
            combined->kind = NODE_EXPR_DIV;
            combined->as.div = div_node;
        } else {
            fprintf(stderr, "Unexpected operator in expression: %d\n", op.type);
            exit(EXIT_FAILURE);
        }

        // make combined expression the new lhs
        lhs_expr = combined;
    }

    return lhs_expr;
}

NodeStmt *parse_stmt(ParserCtx *ctx);

NodeScope *parse_scope(ParserCtx *ctx) {
    expect_token(ctx, TOKEN_OPEN_CURLY);

    NodeScope *scope = malloc(sizeof(NodeScope));
    scope->stmts = NULL;
    scope->stmt_count = 0;

    // parse statements until closing curly brace
    // this will result in infinite loop if no close curly :(
    while (ctx->current_pos < ctx->token_count && ctx->tokens[ctx->current_pos].type != TOKEN_CLOSE_CURLY) {
        NodeStmt *stmt = parse_stmt(ctx);
        scope->stmts = realloc(scope->stmts, sizeof(NodeStmt*) * (scope->stmt_count + 1));
        scope->stmts[scope->stmt_count] = stmt;
        scope->stmt_count++;
    }

    expect_token(ctx, TOKEN_CLOSE_CURLY);

    return scope;
}

NodeStmt *parse_stmt(ParserCtx *ctx) {
    Token *current = &ctx->tokens[ctx->current_pos];
    if (current->type == TOKEN_EXIT) {
        ctx->current_pos++;

        // expect open parenethesis
        expect_token(ctx, TOKEN_OPEN_PAREN);

        // expect expression
        NodeExpr *expr = parse_expr(ctx, 0);

        // expect close parenthesis
        expect_token(ctx, TOKEN_CLOSE_PAREN);

        // expect semicolon
        expect_token(ctx, TOKEN_SEMI);

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
        expect_token(ctx, TOKEN_IDENT);

        // expect equals
        expect_token(ctx, TOKEN_EQUALS);

        // expect expression
        NodeExpr *expr = parse_expr(ctx, 0);

        // expect semicolon
        expect_token(ctx, TOKEN_SEMI);

        NodeStmtVar *stmt_var = malloc(sizeof(NodeStmtVar));
        stmt_var->ident = ident_token;
        stmt_var->expr = expr;

        NodeStmt *stmt_node = malloc(sizeof(NodeStmt));
        stmt_node->kind = NODE_STMT_VAR;
        stmt_node->as.stmt_var = stmt_var;

        return stmt_node;
    } else if (current->type == TOKEN_OPEN_CURLY) {
        ctx->current_pos++;
        // this is kinda messy with `NodeScope` and `NodeStmtScope` but oh well
        NodeScope *scope = parse_scope(ctx);

        NodeStmtScope *stmt_scope = malloc(sizeof(NodeStmtScope));
        stmt_scope->scope = scope;

        NodeStmt *stmt_node = malloc(sizeof(NodeStmt));
        stmt_node->kind = NODE_STMT_SCOPE;
        stmt_node->as.stmt_scope = stmt_scope;

        return stmt_node;
    } else if (current->type == TOKEN_IF) {
        ctx->current_pos++;

        expect_token(ctx, TOKEN_OPEN_PAREN);
        NodeExpr *expr = parse_expr(ctx, 0);
        expect_token(ctx, TOKEN_CLOSE_PAREN);
        NodeScope *scope = parse_scope(ctx);

        NodeStmtIf *stmt_if = malloc(sizeof(NodeStmtIf));
        stmt_if->expr = expr;
        stmt_if->scope = scope;

        NodeStmt *stmt_node = malloc(sizeof(NodeStmt));
        stmt_node->kind = NODE_STMT_IF;
        stmt_node->as.stmt_if = stmt_if;

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