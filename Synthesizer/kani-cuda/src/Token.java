public class Token {

    public String kind;
    public String value;
    public Token left;
    public Token right;

    @Override
    public String toString() {
        return kind + " \"" + value + "\"";
    }

    public String paren() {
        if (left == null && right == null) {
            return value;
        } else {
            StringBuilder b = new StringBuilder();
            b.append("(");
            if (left != null) {
                b.append(left.paren()).append(" ");
            }
            b.append(value);
            if (right != null) {
                b.append(" ").append(right.paren());
            }
            b.append(")");
            return b.toString();
        }
    }

}