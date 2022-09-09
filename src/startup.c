#include <stdio.h>

/* Define all Scheme constants */
#define FX_MASK          0x03
#define FX_TAG           0x00
#define FX_SHIFT            2

#define FALSE            0x2F
#define TRUE             0x6F
#define EMPTY_LIST       0x3F
#define TAB              0x9F
#define NEWLINE          0xAF
#define RETURN           0xDF
#define SPACE            0x20F
#define EXCLAMATION_MARK 0x21F
#define DOUBLE_QUOTES    0x22F
#define POUND            0x23F
#define DOLLAR           0x24F
#define PERCENT          0x25F
#define AMPERSAND        0x26F
#define QUOTE            0x27F
#define OPEN_PAREN       0x28F
#define CLOSE_PAREN      0x29F
#define MUL              0x2AF
#define ADD              0x2BF
#define COMMA            0x2CF
#define SUB              0x2DF
#define PERIOD           0x2EF
#define F_SLASH          0x2FF
#define ZERO             0x30F
#define ONE              0x31F
#define TWO              0x32F
#define THREE            0x33F
#define FOUR             0x34F
#define FIVE             0x35F
#define SIX              0x36F
#define SEVEN            0x37F
#define EIGHT            0x38F
#define NINE             0x39F
#define COLON            0x3AF
#define SEMI_COLON       0x3BF
#define LT               0x3CF
#define EQ               0x3DF
#define GT               0x3EF
#define QUESTION_MARK    0x3FF
#define AT               0x40F
#define U_A              0x41F
#define U_B              0x42F
#define U_C              0x43F
#define U_D              0x44F
#define U_E              0x45F
#define U_F              0x46F
#define U_G              0x47F
#define U_H              0x48F
#define U_I              0x49F
#define U_J              0x4AF
#define U_K              0x4BF
#define U_L              0x4CF
#define U_M              0x4DF
#define U_N              0x4EF
#define U_O              0x4FF
#define U_P              0x50F
#define U_Q              0x51F
#define U_R              0x52F
#define U_S              0x53F
#define U_T              0x54F
#define U_U              0x55F
#define U_V              0x56F
#define U_W              0x57F
#define U_X              0x58F
#define U_Y              0x59F
#define U_Z              0x5AF
#define OPEN_SQUARE      0x5BF
#define B_SLASH          0x5CF
#define CLOSE_SQUARE     0x5DF
#define CARET            0x5EF
#define UNDERSCORE       0x5FF
#define BACKTICK         0x60F
#define L_A              0x61F
#define L_B              0x62F
#define L_C              0x63F
#define L_D              0x64F
#define L_E              0x65F
#define L_F              0x66F
#define L_G              0x67F
#define L_H              0x68F
#define L_I              0x69F
#define L_J              0x6AF
#define L_K              0x6BF
#define L_L              0x6CF
#define L_M              0x6DF
#define L_N              0x6EF
#define L_O              0x6FF
#define L_P              0x70F
#define L_Q              0x71F
#define L_R              0x72F
#define L_S              0x73F
#define L_T              0x74F
#define L_U              0x75F
#define L_V              0x76F
#define L_W              0x77F
#define L_X              0x78F
#define L_Y              0x79F
#define L_Z              0x7AF
#define OPEN_BRACK       0x7BF
#define VERTICAL_LINE    0x7CF
#define CLOSE_BRACK      0x7DF
#define TILDE            0x7EF

/* all scheme values are of type ptrs */
typedef unsigned int ptr;

struct dict
{
    int key;
    char value[9];
} immediates [] =
{
    {FALSE, "#f"},
    {TRUE, "#t"},
    {EMPTY_LIST, "()"},
    {TAB, "#\\tab"},
    {NEWLINE, "#\\newline"},
    {RETURN, "#\\return"},
    {SPACE, "#\\space"},
    {EXCLAMATION_MARK, "#\\!"},
    {DOUBLE_QUOTES, "#\\\""},
    {POUND, "#\\#"},
    {DOLLAR, "#\\$"},
    {PERCENT, "#\\%"},
    {AMPERSAND, "#\\&"},
    {QUOTE, "#\\'"},
    {OPEN_PAREN, "#\\("},
    {CLOSE_PAREN, "#\\)"},
    {MUL, "#\\*"},
    {ADD, "#\\+"},
    {COMMA, "#\\,"},
    {SUB, "#\\-"},
    {PERIOD, "#\\."},
    {F_SLASH, "#\\/"},
    {ZERO, "#\\0"},
    {ONE, "#\\1"},
    {TWO, "#\\2"},
    {THREE, "#\\3"},
    {FOUR, "#\\4"},
    {FIVE, "#\\5"},
    {SIX, "#\\6"},
    {SEVEN, "#\\7"},
    {EIGHT, "#\\8"},
    {NINE, "#\\9"},
    {COLON, "#\\:"},
    {SEMI_COLON, "#\\;"},
    {LT, "#\\<"},
    {EQ, "#\\="},
    {GT, "#\\>"},
    {QUESTION_MARK, "#\\?"},
    {AT, "#\\@"},
    {U_A, "#\\A"},
    {U_B, "#\\B"},
    {U_C, "#\\C"},
    {U_D, "#\\D"},
    {U_E, "#\\E"},
    {U_F, "#\\F"},
    {U_G, "#\\G"},
    {U_H, "#\\H"},
    {U_I, "#\\I"},
    {U_J, "#\\J"},
    {U_K, "#\\K"},
    {U_L, "#\\L"},
    {U_M, "#\\M"},
    {U_N, "#\\N"},
    {U_O, "#\\O"},
    {U_P, "#\\P"},
    {U_Q, "#\\Q"},
    {U_R, "#\\R"},
    {U_S, "#\\S"},
    {U_T, "#\\T"},
    {U_U, "#\\U"},
    {U_V, "#\\V"},
    {U_W, "#\\W"},
    {U_X, "#\\X"},
    {U_Y, "#\\Y"},
    {U_Z, "#\\Z"},
    {OPEN_SQUARE, "#\\["},
    {B_SLASH, "#\\\\"},
    {CLOSE_SQUARE, "#\\]"},
    {CARET, "#\\^"},
    {UNDERSCORE, "#\\_"},
    {BACKTICK, "#\\`"},
    {L_A, "#\\a"},
    {L_B, "#\\b"},
    {L_C, "#\\c"},
    {L_D, "#\\d"},
    {L_E, "#\\e"},
    {L_F, "#\\f"},
    {L_G, "#\\g"},
    {L_H, "#\\h"},
    {L_I, "#\\i"},
    {L_J, "#\\j"},
    {L_K, "#\\k"},
    {L_L, "#\\l"},
    {L_M, "#\\m"},
    {L_N, "#\\n"},
    {L_O, "#\\o"},
    {L_P, "#\\p"},
    {L_Q, "#\\q"},
    {L_R, "#\\r"},
    {L_S, "#\\s"},
    {L_T, "#\\t"},
    {L_U, "#\\u"},
    {L_V, "#\\v"},
    {L_W, "#\\w"},
    {L_X, "#\\x"},
    {L_Y, "#\\y"},
    {L_Z, "#\\z"},
    {OPEN_BRACK, "#\\{"},
    {VERTICAL_LINE, "#\\|"},
    {CLOSE_BRACK, "#\\}"},
    {TILDE, "#\\~"}
};

int scheme_entry(); // fix implicit function declaration error

// Looks up the key index from the dictionary dict.
int lookup(struct dict d[], int len, int x)
{
    for(int i = 0; i < len; ++i)
    {
        if (x == d[i].key)
            return i;
    }

    return -1;
}

static void print_ptr(ptr x)
{
    if((x & FX_MASK) == FX_TAG)
    {
        printf("%d", ((int)x) >> FX_SHIFT);
    }
    else
    {
        int len = sizeof(immediates) / sizeof(immediates[0]);
        int result = lookup(immediates, len, x);

        if(result != -1)
        {
            printf("%s", immediates[result].value);
        }
        else
        {
            printf("#<unknown 0x%08x", x);
        }
    }

    printf("\n");
}

int main(int argc, char** argv)
{
    print_ptr(scheme_entry());

    return 0;
}
