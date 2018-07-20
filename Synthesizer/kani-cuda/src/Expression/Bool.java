package Expression;

public class Bool extends AExpression{
	public Bool() {
		super(0);
	}
	
	public Bool(String op, Expression exp) {
		super(0);
		new UnOpBool(op, exp);
	}
	
	public Bool(String op, Expression left, Expression right) {
		super(0);
		new BinOpBool(op, left, right);
	}
	
}
