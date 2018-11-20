package Expression;

public class Constant<T> extends AExpression{
	T val;

	public Constant(T val) {
		super(0);
		this.val = val;
	}
	
	public String toStringExp(){
		return this.val.toString();
	}
	
	public int eval(Environment env){
		return (int)this.val;
	}
	
	public boolean isConstant(){
		return true;
	}
	

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((val == null) ? 0 : val.hashCode());
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
		Constant other = (Constant) obj;
		if (val == null) {
			if (other.val != null)
				return false;
		} else if (!val.equals(other.val))
			return false;
		return true;
	}

	public int contain(int num, Expression that) {
		if(this.equals(that)) {
			return num;
		} else {
			return 0;
		}
	}

	@Override
	public boolean isReverse(Expression that) {
		return false;
	}
	
}
