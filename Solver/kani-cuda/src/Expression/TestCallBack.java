package Expression;

import java.util.Arrays;

public class TestCallBack implements CallBack {
	static int count;
	static boolean test = false;
	
	static int count_logic;
	static int limit = Integer.MAX_VALUE;
	static Cursor cursor = new Cursor();
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		count++;
		
		System.out.print(Arrays.toString(exp.getExpression()));
		System.out.println(" " + index 
					//+ " " + Arrays.toString(limit) 
					+ " " + exp.evaluate(index));
		
	}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		count_logic++;
		cursor.setIndex(0);
		//System.out.print(Arrays.toString(exp.getExpression()));
		//System.out.println(" " + index + " " + exp.evaluate(cursor));
	}

}