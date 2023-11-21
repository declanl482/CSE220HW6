#include "../include/hw6.h"

int is_uppercase(char ch) {
    if (ch >= 'A' && ch <= 'Z') {
        return 1;
    }
    
    return 0;
}

int is_matrix_operator(char ch) {
    if (ch == '\'' || ch == '*' || ch == '+') {
        return 1;
    }
    
    return 0;
}

int is_eval_expr(char *line) {
    char *line_ptr = line;
    while (*line_ptr != '\0') {
        if (isspace(*line_ptr)) {
            line_ptr++;
            continue;
        }
        else if (!is_uppercase(*line_ptr) && !is_matrix_operator(*line_ptr) && *line_ptr != '=' && *line_ptr != '(' && *line_ptr != ')') {
            return 0;
        }
        line_ptr++;
    }
    return 1;
}

int compute_postfix_len(char *infix) {
    char *infix_ptr = infix;
    int len = 0;
    while (*infix_ptr != '\0') {
        if (isspace(*infix_ptr) || *infix_ptr == '(' || *infix_ptr == ')') {
            infix_ptr++;
            continue;
        }
        else if (is_uppercase(*infix_ptr) || is_matrix_operator(*infix_ptr)) {
            len++;
        }
        infix_ptr++;
    }
    
    return len;
}

int precedence(char operator) {
    switch(operator) {
        case '\'': return 3;
        case '*': return 2;
        case '+': return 1;
        case '(': return 0;
        default: return -1;
    }
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        bst_sf *res_root = (bst_sf *)malloc(sizeof(bst_sf));
        res_root->mat = mat;
        res_root->left_child = NULL;
        res_root->right_child = NULL;

        return res_root;
    }
    int cmp = mat->name - root->mat->name;

    if (cmp < 0) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    else if (cmp > 0) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }

    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    matrix_sf *res_mat = NULL;

    if (root == NULL) {
        res_mat = NULL;
    }
    else if (name == root->mat->name) {
        res_mat = root->mat;
    }
    else if (name < root->mat->name) {
        res_mat = find_bst_sf(name, root->left_child);
    }
    else if (name > root->mat->name) {
        res_mat = find_bst_sf(name, root->right_child);
    }

    return res_mat;
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }

    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int mat1_rows = mat1->num_rows;
    unsigned int mat2_cols = mat2->num_cols;

    matrix_sf *res_mat = (matrix_sf *)malloc(sizeof(matrix_sf) + mat1_rows * mat2_cols * sizeof(int));
    res_mat->name = '?';
    res_mat->num_rows = mat1_rows;
    res_mat->num_cols = mat2_cols;

    for (int idx = 0; idx < mat1_rows * mat2_cols; idx++) {
        res_mat->values[idx] = mat1->values[idx] + mat2->values[idx];
    }

    return res_mat;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int mat1_rows = mat1->num_rows;
    unsigned int mat1_cols = mat1->num_cols;
    unsigned int mat2_cols = mat2->num_cols;

    matrix_sf *res_mat = (matrix_sf *)malloc(sizeof(matrix_sf) + mat1_rows * mat2_cols * sizeof(int));
    res_mat->name = '?';
    res_mat->num_rows = mat1_rows;
    res_mat->num_cols = mat2_cols;

    for (int row = 0; row < mat1_rows; row++) {
        for (int col = 0; col < mat2_cols; col++) {
            res_mat->values[row * mat2_cols + col] = 0;
            for (int idx = 0; idx < mat1_cols; idx++) {
                res_mat->values[row * mat2_cols + col] += mat1->values[row * mat1_cols + idx] * mat2->values[idx * mat2_cols + col];
            }
        }
    }

    return res_mat;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    unsigned int mat_rows = mat->num_rows;
    unsigned int mat_cols = mat->num_cols;

    matrix_sf *res_mat = (matrix_sf *)malloc(sizeof(matrix_sf) + mat_cols * mat_rows * sizeof(int));
    res_mat->name = '?';
    res_mat->num_rows = mat_cols;
    res_mat->num_cols = mat_rows;

    for (int row = 0; row < mat_rows; row++) {
        for (int col = 0; col < mat_cols; col++) {
            res_mat->values[col * mat_rows + row] = mat->values[row * mat_cols + col];
        }
    }

    return res_mat;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    
    char *expr_ptr = expr;

    while (isspace(*expr_ptr)) {
        expr_ptr++;
    }
    
    int rows = strtol(expr_ptr, &expr_ptr, 10);

    while (isspace(*expr_ptr)) {
        expr_ptr++;
    }

    int cols = strtol(expr_ptr, &expr_ptr, 10);

    matrix_sf *res_mat = (matrix_sf *)malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    res_mat->name = name;
    res_mat->num_rows = rows;
    res_mat->num_cols = cols;

    while (isspace(*expr_ptr) || *expr_ptr == '[') {
        expr_ptr++;
    }

    int idx = 0;
    while (*expr_ptr != ']') {
        if (isspace(*expr_ptr) || *expr_ptr == ';') {
            expr_ptr++;
            continue;
        }
        int value = strtol(expr_ptr, &expr_ptr, 10);
        res_mat->values[idx++] = value;
    }

    return res_mat;
}

char* infix2postfix_sf(char *infix) {
    int stack_len = strlen(infix);
    char stack[stack_len];
    int top = -1;

    int postfix_len = compute_postfix_len(infix);
    char *postfix = (char *)malloc((postfix_len + 1) * sizeof(char));
    char *postfix_ptr = postfix;

    char *infix_ptr = infix;
    while (*infix_ptr != '\0' && top < stack_len) {
        if (isspace(*infix_ptr)) {
            infix_ptr++;
            continue;
        }
        else if (is_uppercase(*infix_ptr)) {
            *postfix_ptr = *infix_ptr;
            postfix_ptr++;
            infix_ptr++;
            continue;
        }
        else if (*infix_ptr == '(') {
            stack[++top] = *infix_ptr;
            infix_ptr++;
            continue;
        }
        else if (*infix_ptr == ')') {
            while (top > -1 && stack[top] != '(') {
                *postfix_ptr = stack[top--];
                postfix_ptr++;
            }
            top--;
        }
        else if (is_matrix_operator(*infix_ptr)) {
            while (top > -1 && precedence(stack[top]) >= precedence(*infix_ptr)) {
                *postfix_ptr = stack[top--];
                postfix_ptr++;
            }
            stack[++top] = *infix_ptr;
        }
        infix_ptr++;
    }

    while (top < stack_len && top > -1) {
        *postfix_ptr = stack[top--];
        postfix_ptr++;
    }
    *postfix_ptr = '\0';

    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    char *postfix_ptr = postfix;

    int stack_len = strlen(postfix);
    matrix_sf *stack[stack_len];
    int top = -1;

    matrix_sf *res_mat = NULL;
    while (*postfix_ptr != '\0' && top < stack_len) {
        if (is_uppercase(*postfix_ptr)) {
            stack[++top] = find_bst_sf(*postfix_ptr, root);
        }
        else if (*postfix_ptr == '\'') {
            matrix_sf *op_1 = stack[top--];
            stack[++top] = transpose_mat_sf(op_1);
            if (op_1->name == '?') {
                free(op_1);
            }
        }
        else if (*postfix_ptr == '*') {
            matrix_sf *op_2 = stack[top--];
            matrix_sf *op_1 = stack[top--];
            stack[++top] = mult_mats_sf(op_1, op_2);
            if (op_1->name == '?') {
                free(op_1);
            }
            if (op_2->name == '?') {
                free(op_2);
            }
        }
        else if (*postfix_ptr == '+') {
            matrix_sf *op_2 = stack[top--];
            matrix_sf *op_1 = stack[top--];
            stack[++top] = add_mats_sf(op_1, op_2);
            if (op_1->name == '?') {
                free(op_1);
            }
            if (op_2->name == '?') {
                free(op_2);
            }
        }
        postfix_ptr++;
    }

    if (top < stack_len && top > -1) {
        res_mat = stack[top--];
        res_mat->name = name;
    }
    
    free(postfix);

    return res_mat;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        return NULL;
    }

    bst_sf *root = NULL;
    matrix_sf *temp = NULL;
    char *line = NULL;
    size_t max_line_size = MAX_LINE_LEN + 1;

    while (getline(&line, &max_line_size, file) != -1) {
        
        char buffer[strlen(line) + 1];
        memcpy(buffer, line, strlen(line));
        buffer[strlen(line)] = '\0';
        char l_value;
        char *r_value;

        char *token = strtok(buffer, "=");
        if (token != NULL) {
            l_value = *token;
        }

        token = strtok(NULL, "=");
        if (token != NULL) {
            r_value = token;
        }

        if (is_eval_expr(line)) {
            matrix_sf *eval_mat = evaluate_expr_sf(l_value, r_value, root);
            temp = eval_mat;
            insert_bst_sf(eval_mat, root);
        }
        else {
            matrix_sf *new_mat = create_matrix_sf(l_value, r_value);
            if (root == NULL) {
                root = insert_bst_sf(new_mat, root);
            }
            else {
                insert_bst_sf(new_mat, root);
            }
        }
    }

    matrix_sf *res = copy_matrix(temp->num_rows, temp->num_cols, temp->values);
    res->name = temp->name;
    free(line);
    free_bst_sf(root);
    fclose(file);
    return res;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}