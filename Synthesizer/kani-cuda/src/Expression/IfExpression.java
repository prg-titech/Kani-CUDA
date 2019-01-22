package Expression;

public class IfExpression extends AIfExpression{
	BoolExpression cond;
	Expression thn;
	Expression els;
	
	public IfExpression(BoolExpression cond, Expression thn, Expression els) {
		this.cond = cond;
		this.thn = thn;
		this.els = els;
	}
	
	public int eval(Environment env){
		if (this.cond.eval(env)) {
			return this.thn.eval(env);
		} else {
			return this.els.eval(env);
		}
	}
	
	public String toStringExp(){
		return this.cond.toStringExp() + " ? " + this.thn.toStringExp() + " : "  + this.els.toStringExp();
	}
	
	public void print(){
		System.out.println(this.toStringExp());
	}
}
