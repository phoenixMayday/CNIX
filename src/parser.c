#include "lexer.c"

// Terms
typedef enum {
    NODE_TERM_INT_LIT
} NodeTermKind;

typedef struct {
    Token int_lit;
} NodeTermIntLit;

typedef struct {
    NodeTermKind kind;
    union {
        NodeTermIntLit *int_lit;
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
    NODE_STMT_EXIT
} NodeStmtKind;

typedef struct {
    NodeExpr *expr;
} NodeStmtExit;

typedef struct {
    NodeStmtKind kind;
    union {
        NodeStmtExit *exit_stmt;
    } as;
} NodeStmt;

typedef struct {
    NodeStmt **stmts;
    int stmt_count;
} NodeProg;


NodeTerm *parse_term(Token **tokens, int *token_pos, int token_count) {
    Token *current = &(*tokens)[*token_pos];
    if (current->type == TOKEN_INT_LIT) {
        NodeTermIntLit *int_lit_node = malloc(sizeof(NodeTermIntLit));
        int_lit_node->int_lit = *current;

        NodeTerm *term_node = malloc(sizeof(NodeTerm));
        term_node->kind = NODE_TERM_INT_LIT;
        term_node->as.int_lit = int_lit_node;

        (*token_pos)++;
        return term_node;
    }
    else {
        fprintf(stderr, "Unexpected token in term\n");
        exit(EXIT_FAILURE);
    }
}

NodeExpr *parse_expr(Token **tokens, int *token_pos, int token_count) {
    // eval left term
    NodeTerm *lhs_term = parse_term(tokens, token_pos, token_count);
    NodeExpr *lhs_expr = malloc(sizeof(NodeExpr));
    lhs_expr->kind = NODE_EXPR_TERM;
    lhs_expr->as.term = lhs_term ;
    
    if (*token_pos == token_count-1) {
        // if at end, return term as expr
        return lhs_expr;
    } else if (*token_pos == token_count-2) {
        // if 1 token left, error (needs to have 2: operator and rhs)
        fprintf(stderr, "Incomplete expression\n");
        exit(EXIT_FAILURE);
    }

    // eval operator
    Token *op = &(*tokens)[*token_pos];
    (*token_pos)++;

    NodeExpr *result_expr = malloc(sizeof(NodeExpr));
    if (op->type == TOKEN_PLUS) {
        NodeExprAdd *add_node = malloc(sizeof(NodeExprAdd));
        add_node->lhs = lhs_expr;
        // eval right expression
        add_node->rhs = parse_expr(tokens, token_pos, token_count);
        
        result_expr->kind = NODE_EXPR_ADD;
        result_expr->as.add = add_node;
    } else if (op->type == TOKEN_MINUS) {
        NodeExprSub *sub_node = malloc(sizeof(NodeExprSub));
        sub_node->lhs = lhs_expr;
        // eval right expression
        sub_node->rhs = parse_expr(tokens, token_pos, token_count);
        
        result_expr->kind = NODE_EXPR_SUB;
        result_expr->as.sub = sub_node;
    } else {
        fprintf(stderr, "Unexpected operator in expression: %d\n", op->type);
        exit(EXIT_FAILURE);
    }

    return result_expr;
}

NodeStmt *parse_stmt(Token **tokens, int *token_pos, int token_count) {
    Token *current = &(*tokens)[*token_pos];
    if (current->type == TOKEN_EXIT) {
        (*token_pos)++;

        // expect expression
        NodeExpr *expr = parse_expr(tokens, token_pos, token_count);

        // expect semicolon
        if (*token_pos >= token_count || (*tokens)[*token_pos].type != TOKEN_SEMI) {
            fprintf(stderr, "Expected ';' at end of statement\n");
            exit(EXIT_FAILURE);
        }
        (*token_pos)++;

        NodeStmtExit *exit_node = malloc(sizeof(NodeStmtExit));
        exit_node->expr = expr;

        NodeStmt *stmt_node = malloc(sizeof(NodeStmt));
        stmt_node->kind = NODE_STMT_EXIT;
        stmt_node->as.exit_stmt = exit_node;

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

    int token_pos = 0;
    while (token_pos < token_count) {
        NodeStmt *stmt = parse_stmt(tokens, &token_pos, token_count);
        prog->stmts = realloc(prog->stmts, sizeof(NodeStmt*) * (prog->stmt_count + 1));
        prog->stmts[prog->stmt_count] = stmt;
        prog->stmt_count++;
        printf("Parsed statement, total count: %d\n", prog->stmt_count);
        printf("Current token position: %d / %d\n", token_pos, token_count);
    }

    return prog;
}