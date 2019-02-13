package Expression;
public class Variable extends AExpression{
	String name;

	public Variable(String name) {
		super(1);
		this.name = name;
	}
	
	public String getName(){
		return this.name;
	}
	
	public String toStringExp(){
		return this.name;
	}

	public int eval(Environment env){
		return env.getVal(this);
	}
	
	public boolean isVariable(){
		return true;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		Variable other = (Variable) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
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
