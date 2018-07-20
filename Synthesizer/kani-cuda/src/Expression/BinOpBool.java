package Expression;

public class BinOpBool extends AExpression{
	String op;
	Expression left;
	Expression right;
	
	public BinOpBool(String op, Expression left, Expression right) {
		super(0);
		this.op = op;
		this.left = left;
		this.right = right;
	}
	
	public String toStringExp(){
		return "(" + this.left.toStringExp() + op + this.right.toStringExp() + ")";
	}
	
	
	/*
	public boolean eval(Environment env){
		if (op.equals("&&")) {
			return this.left.eval(env) && this.right.eval(env);
		} else if (op.equals("||")){
			return this.left.eval(env) || this.right.eval(env);
		} else if (op.equals("==")) {
			return this.left.eval(env) == this.right.eval(env);
		} else {
			return false;
		}
	}
	*/
}
