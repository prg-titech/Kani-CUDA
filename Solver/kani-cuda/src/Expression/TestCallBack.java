package Expression;

public class TestCallBack implements CallBack {
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		exp.test(index);
	}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		exp.test(index);
	}
}