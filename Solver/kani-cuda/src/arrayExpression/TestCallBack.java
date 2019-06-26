package arrayExpression;

import java.util.Arrays;

public class TestCallBack implements CallBack{
	public static int count;
	public void call(int index, arrExpression exp) {
		System.out.println(Arrays.toString(exp.getExpression()));
		count++;
	}
}