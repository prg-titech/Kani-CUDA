package Expression;

public class BinOpArith extends ABoolExpression {
	String op;
	Expression left;
	Expression right;
	
	public BinOpArith(String op, Expression left, Expression right) {
		this.op = op;
		this.left = left;
		this.right = right;
	}
	
	public String toStringExp(){
		return "(" + this.left.toStringExp() + op + this.right.toStringExp() + ")";
	}
	
	public boolean eval(Environment env){
		if (op.equals("<")) {
			return this.left.eval(env) < this.right.eval(env);
		} else if (op.equals(">")){
			return this.left.eval(env) > this.right.eval(env);
		} else if (op.equals("==")) {
			return this.left.eval(env) == this.right.eval(env);
		} else {
			return false;
		}
	}
}
