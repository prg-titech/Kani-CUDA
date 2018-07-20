import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Interpreter {

    public Map<String, Integer> variables;
    List<Token> body;

    public Interpreter init(List<Token> body) {
        variables = new HashMap<>();
        this.body = body;
        return this;
    }

    public Map<String, Integer> run() throws Exception {
        body(body);
        return variables;
    }

    public void body(List<Token> body) throws Exception {
        for (Token exprs : body) {
            expression(exprs);
        }
    }

    public Object expression(Token expr) throws Exception {
        if (expr.kind.equals("digit")) {
            return digit(expr);
        } else if (expr.kind.equals("variable")) {
            return var(expr);
        } else if (expr.kind.equals("sign") && expr.value.equals("=")) {
            return assign(expr);
        } else if (expr.kind.equals("sign")) {
            return calc(expr);
        } else {
            throw new Exception("Expression error");
        }
    }

    public Integer digit(Token token) {
        return Integer.decode(token.value);
    }

    public Object var(Token token) {
        String name = token.value;
        if (!variables.containsKey(name)) {
            variables.put(name, 0);
        }
        return name;
    }

    public String assign(Token expr) throws Exception {
        String name = variable(expression(expr.left));
        Integer value = value(expression(expr.right));
        variables.put(name, value);
        return name;
    }

    public String variable(Object value) throws Exception {
        if (value instanceof String) {
            return (String) value;
        } else {
            throw new Exception("left value error");
        }
    }

    public Integer value(Object value) throws Exception {
        if (value instanceof Integer) {
            return (Integer) value;
        } else if (value instanceof String) {
            return variables.get((String) value);
        } else {
            throw new Exception("right value error");
        }
    }

    public Object calc(Token expr) throws Exception {
        Integer left = value(expression(expr.left));
        Integer right = value(expression(expr.right));
        if (expr.value.equals("+")) {
            return left + right;
        } else if (expr.value.equals("-")) {
            return left - right;
        } else if (expr.value.equals("*")) {
            return left * right;
        } else if (expr.value.equals("/")) {
            return left / right;
        } else {
            throw new Exception("Unknown sign for Calc");
        }
    }

    public static void main(String[] args) throws Exception {
        String text = "a = 3 + 4 - 5";
        List<Token> tokens = new Lexer().init(text).tokenize();
        List<Token> blk = new Parser().init(tokens).block();
        Map<String, Integer> variables = new Interpreter().init(blk).run();
        System.out.println(variables);
        for (Map.Entry<String, Integer> variable : variables.entrySet()) {
            System.out.println(variable.getKey() + " " + variable.getValue());
        }
        // --> a 23
    }
}