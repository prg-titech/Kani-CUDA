package Expression;

public class PartialCallBack implements CallBack {
	
	private int max_error;
	
	public PartialCallBack(int max) {
		max_error = max;
	}
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		exp.testPartial(index, max_error);
	}

	public void call(int index, int limit, LinearLogicExpression exp) {}

}