package Expression;

public class TermCallBack implements CallBack {
	
	public CallBack cb;
	
	public TermCallBack(CallBack cb) {
		this.cb = cb;
	}
	
	public void call(int index, int[] limit, LinearArithExpression exp) {
		//limit[1] = exp.getM() - 1;
		if (limit[2] <= 0) { return; }
		limit[2] = limit[2] - 1;
		exp.arr[index] = 5;
		exp.gen_term(index + 1, limit, cb);
	}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		return;
	}
}