package Expression;

import java.util.Arrays;

public class TestCallBack implements CallBack {
	static int count;
	static boolean test = false;
	
	static int count_logic;
	static int limit = Integer.MAX_VALUE;
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		count++;
		
		//System.out.print(Arrays.toString(exp.getExpression()));
		exp.test(index);
	}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		count_logic++;
		exp.test(index);
	}
}