package Expression;

public abstract class ABoolExpression implements BoolExpression {
	public BoolExpression unOp(String op){
		return new UnOpBool(op, this);
	}
	
	public BoolExpression binOp(String op, BoolExpression that){
		return new BinOpBool(op, this, that);
	}
	
	public String toStringExp(){
		return "";
	}
	
	public void print(){
		System.out.println(this.toStringExp());
	}
	
	public boolean eval(Environment env){
		return false;
	}
}
