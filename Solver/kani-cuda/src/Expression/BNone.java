package Expression;

public class BNone implements BoolExpression {

	@Override
	public BoolExpression unOp(String op) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public BoolExpression binOp(String op, BoolExpression that) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void print() {
		// TODO Auto-generated method stub
		System.out.println("bnone");

	}

	@Override
	public String toStringExp() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean eval(Environment env) {
		// TODO Auto-generated method stub
		return false;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return true;
	}

}
