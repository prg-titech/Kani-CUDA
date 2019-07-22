package Expression;

public class TestCallBack implements CallBack {
	static int count;
	
	static int count_logic;
	static int limit = Integer.MAX_VALUE;
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		//count++;
		exp.test(index);
	}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		//count_logic++;
		exp.test(index);
	}
}