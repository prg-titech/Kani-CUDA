package Expression;

public class SumCallBack implements CallBack {
	
	public CallBack cb;
	
	public SumCallBack(CallBack cb) {
		this.cb = cb;
	}
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		if (limit[2] <= 0) { return; }
		limit[1] = exp.getM() - 2;
		limit[2] = limit[2] - 1;
		exp.arr[index] = 3;
		limit[0] = limit[3];
		limit[3] = 0;
		exp.gen_sum(index + 1, limit, cb);
		exp.arr[index] = 4;
		limit[0] = Integer.MAX_VALUE;
		exp.gen_minus(index + 1, limit, cb);
	}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		return;
	}
}