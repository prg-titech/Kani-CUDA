package Expression;

public class UnOpBool extends AExpression{
	String op;
	Expression exp;
	
	public UnOpBool(String op, Expression exp) {
		super(0);
		this.op = op;
		this.exp = exp;
	}

	public String toStringExp(){
		return op + this.exp.toStringExp();
	}
}
