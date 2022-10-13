#include <stdio.h>

/* Define all Scheme constants */
#define FX_MASK          0x03
#define FX_TAG           0x00
#define FX_SHIFT            2

#define FALSE            0x2F
#define TRUE             0x6F
#define EMPTY_LIST       0x3F
#define TAB              0x90F
#define NEWLINE          0xA0F
#define RETURN           0xD0F
#define SPACE            0x200F
#define EXCLAMATION_MARK 0x210F
#define DOUBLE_QUOTES    0x220F
#define POUND            0x230F
#define DOLLAR           0x240F
#define PERCENT          0x250F
#define AMPERSAND        0x260F
#define QUOTE            0x270F
#define OPEN_PAREN       0x280F
#define CLOSE_PAREN      0x290F
#define MUL              0x2A0F
#define ADD              0x2B0F
#define COMMA            0x2C0F
#define SUB              0x2D0F
#define PERIOD           0x2E0F
#define F_SLASH          0x2F0F
#define ZERO             0x300F
#define ONE              0x310F
#define TWO              0x320F
#define THREE            0x330F
#define FOUR             0x340F
#define FIVE             0x350F
#define SIX              0x360F
#define SEVEN            0x370F
#define EIGHT            0x380F
#define NINE             0x390F
#define COLON            0x3A0F
#define SEMI_COLON       0x3B0F
#define LT               0x3C0F
#define EQ               0x3D0F
#define GT               0x3E0F
#define QUESTION_MARK    0x3F0F
#define AT               0x400F
#define U_A              0x410F
#define U_B              0x420F
#define U_C              0x430F
#define U_D              0x440F
#define U_E              0x450F
#define U_F              0x460F
#define U_G              0x470F
#define U_H              0x480F
#define U_I              0x490F
#define U_J              0x4A0F
#define U_K              0x4B0F
#define U_L              0x4C0F
#define U_M              0x4D0F
#define U_N              0x4E0F
#define U_O              0x4F0F
#define U_P              0x500F
#define U_Q              0x510F
#define U_R              0x520F
#define U_S              0x530F
#define U_T              0x540F
#define U_U              0x550F
#define U_V              0x560F
#define U_W              0x570F
#define U_X              0x580F
#define U_Y              0x590F
#define U_Z              0x5A0F
#define OPEN_SQUARE      0x5B0F
#define B_SLASH          0x5C0F
#define CLOSE_SQUARE     0x5D0F
#define CARET            0x5E0F
#define UNDERSCORE       0x5F0F
#define BACKTICK         0x600F
#define L_A              0x610F
#define L_B              0x620F
#define L_C              0x630F
#define L_D              0x640F
#define L_E              0x650F
#define L_F              0x660F
#define L_G              0x670F
#define L_H              0x680F
#define L_I              0x690F
#define L_J              0x6A0F
#define L_K              0x6B0F
#define L_L              0x6C0F
#define L_M              0x6D0F
#define L_N              0x6E0F
#define L_O              0x6F0F
#define L_P              0x700F
#define L_Q              0x710F
#define L_R              0x720F
#define L_S              0x730F
#define L_T              0x740F
#define L_U              0x750F
#define L_V              0x760F
#define L_W              0x770F
#define L_X              0x780F
#define L_Y              0x790F
#define L_Z              0x7A0F
#define OPEN_BRACK       0x7B0F
#define VERTICAL_LINE    0x7C0F
#define CLOSE_BRACK      0x7D0F
#define TILDE            0x7E0F

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
