package Expression;

public abstract class AIfExpression implements Expression {

	@Override
	public Expression add(Expression that) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression subtract(Expression that) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression multiple(Expression that) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public BoolExpression binOp(String op, Expression that) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isVariable() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isConsatnt() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public int eval(Environment env) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String toStringExp() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void print() {
		// TODO Auto-generated method stub

	}

	@Override
	public int getOrder() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int contain(int num, Expression that) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean isReverse(Expression that) {
		// TODO Auto-generated method stub
		return false;
	}

}
