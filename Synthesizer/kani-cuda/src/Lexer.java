import java.util.ArrayList;
import java.util.List;

public class Lexer {

    private String text;
    private int i;

    public Lexer init(String text) {
        i = 0;
        this.text = text;
        return this;
    }

    private boolean isEOT() {
        return text.length() <= i;
    }

    private char c() throws Exception {
        if (isEOT()) {
            throw new Exception("No more character");
        }
        return text.charAt(i);
    }

    private char next() throws Exception {
        char c = c();
        ++i;
        return c;
    }

    private void skipSpace() throws Exception {
        while (!isEOT() && Character.isWhitespace(c())) {
            next();
        }
    }

    private boolean isSignStart(char c) {
        return c == '=' || c == '+' || c == '-' || c == '*' || c == '/';
    }

    private boolean isDigitStart(char c) throws Exception {
        return Character.isDigit(c);
    }

    private boolean isVariableStart(char c) throws Exception {
        return Character.isAlphabetic(c);
    }

    private Token sign() throws Exception {
        Token t = new Token();
        t.kind = "sign";
        t.value = Character.toString(next());
        return t;
    }

    private Token digit() throws Exception {
        StringBuilder b = new StringBuilder();
        b.append(next());
        while (!isEOT() && Character.isDigit(c())) {
            b.append(next());
        }
        Token t = new Token();
        t.kind = "digit";
        t.value = b.toString();
        return t;
    }

    private Token variable() throws Exception {
        StringBuilder b = new StringBuilder();
        b.append(next());
        while (!isEOT() && (Character.isAlphabetic(c()) || Character.isDigit(c()))) {
            b.append(next());
        }
        Token t = new Token();
        t.kind = "variable";
        t.value = b.toString();
        return t;
    }

    public Token nextToken() throws Exception {
        skipSpace();
        if (isEOT()) {
            return null;
        } else if (isSignStart(c())) {
            return sign();
        } else if (isDigitStart(c())) {
            return digit();
        } else if (isVariableStart(c())) {
            return variable();
        } else {
            throw new Exception("Not a character for tokens");
        }
    }

    public List<Token> tokenize() throws Exception {
        List<Token> tokens = new ArrayList<>();
        Token t = nextToken();
        while (t != null) {
            tokens.add(t);
            t = nextToken();
        }
        return tokens;
    }

    public static void main(String[] args) throws Exception {
        String text = " ans1 = 10 + 20 ";
        List<Token> tokens = new Lexer().init(text).tokenize();
        for (Token token : tokens) {
            System.out.println(token.toString());
        }
        // --> variable ,"ans1"
        // --> sign "="
        // --> digit "10"
        // --> sign "+"
        // --> digit "20"
    }

}