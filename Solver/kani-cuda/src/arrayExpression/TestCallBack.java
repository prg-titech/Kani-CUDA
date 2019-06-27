package arrayExpression;

import java.util.Arrays;

public class TestCallBack implements CallBack{
	public static int count;
	public void call(int index, arrExpression exp) {
		//System.out.print(Arrays.toString(exp.getExpression()));
		//System.out.print(" ");
		//System.out.println(exp.evaluate(new Cursor(0)));
		count++;
	}
}