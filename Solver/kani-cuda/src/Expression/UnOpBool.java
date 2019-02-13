package Expression;

public class UnOpBool extends ABoolExpression{
	String op;
	BoolExpression exp;
	
	public UnOpBool(String op, BoolExpression exp) {
		this.op = op;
		this.exp = exp;
	}

	public String toStringExp(){
		return op + this.exp.toStringExp();
	}
	
	public boolean eval(Environment env){
		if (this.op.equals("!")) {
			return !(this.exp.eval(env));
		} else {
			return false;
		}
	}
}
