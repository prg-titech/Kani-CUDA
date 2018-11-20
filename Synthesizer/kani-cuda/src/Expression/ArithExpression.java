package Expression;

public class ArithExpression extends AExpression{
	String op;
	Expression left;
	Expression right;
	
	public ArithExpression(String op, Expression left, Expression right) {
		super((op.equals("*")) 
				? (left.getOrder() + right.getOrder()) 
				: Math.max(left.getOrder(), right.getOrder()));
		this.op = op;
		this.left = left;
		this.right = right;
	}
	
	public String toStringExp(){
		return "(" + this.left.toStringExp() + " " + op + " " + this.right.toStringExp() + ")";
	}
	
	public int eval(Environment env){
		if (op.equals("+")) {
			return this.left.eval(env) + this.right.eval(env);
		} else if (op.equals("-")){
			return this.left.eval(env) - this.right.eval(env);
		} else if (op.equals("*")) {
			return this.left.eval(env) * this.right.eval(env);
		} else {
			return 0;
		}
	}
	
	public int contain(int num, Expression that){
		if (this.op == "+") {
			return this.left.contain(num, that) + this.right.contain(num, that);
		} else if (this.op == "-") {
			return this.left.contain(num, that) + this.right.contain(-num, that);
		} else {
			if(this.equals(that) || this.isReverse(that)) {
				return num;
			} else {
				return 0;
			}
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((left == null) ? 0 : left.hashCode());
		result = prime * result + ((op == null) ? 0 : op.hashCode());
		result = prime * result + ((right == null) ? 0 : right.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		ArithExpression other = (ArithExpression) obj;
		if (left == null) {
			if (other.left != null)
			return false;
		} else if (!left.equals(other.left))
			return false;
		if (op == null) {
			if (other.op != null)
				return false;
		} else if (!op.equals(other.op))
			return false;
		if (right == null) {
			if (other.right != null)
				return false;
		} else if (!right.equals(other.right))
			return false;
		return true;
	}

	@Override
	public boolean isReverse(Expression that) {
		if(this.op == "+" || this.op == "-"){
			return false;
		} else if(this.op == "*") {
			Expression rev = new ArithExpression("*", this.right, this.left);
			return rev.equals(that);
		} else {
			return false;
		}
	}
}
