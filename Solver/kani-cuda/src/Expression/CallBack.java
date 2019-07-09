package Expression;

public interface CallBack {
	public void call(int index, int[] limit, LinearArithExpression exp);
	public void call(int index, int limit, LinearLogicExpression exp);
}