package Expression;

public abstract class AExpression implements Expression{
	int order;
	
	public AExpression(int order) {
		super();
		this.order = order;
	}

	public String toStringExp() {
		return null;
	}
	
	public void print() {
		System.out.println(this.toStringExp());
	}
	
	public Expression add(Expression that) {
		return new ArithExpression("+", this, that);
	}

	public Expression subtract(Expression that) {
		return new ArithExpression("-", this, that);
	}
	
	public Expression multiple(Expression that) {
		Expression one = new Constant(1);
		if (one.equals(this)) {
			return that;
		} else if (one.equals(that)) {
			return this;
		} else {
			return new ArithExpression("*", this, that);
		}
	}
	
	public BoolExpression binOp(String op, Expression that){
		return new BinOpArith(op, this, that);
	}

	public int eval(Environment env) {
		return 0;
	}

	public boolean isVariable() {
		return false;
	}

	public boolean isConsatnt() {
		return false;
	}
	
	public int getOrder(){
		return this.order;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + order;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AExpression other = (AExpression) obj;
		if (order != other.order)
			return false;
		return true;
	}
}
