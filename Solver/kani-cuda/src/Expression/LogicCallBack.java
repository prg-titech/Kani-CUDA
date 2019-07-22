package Expression;

public class LogicCallBack implements CallBack {
	
	public CallBack cb;
	
	public LogicCallBack(CallBack cb) {
		this.cb = cb;
	}
	
	public void call(int index, int[] limit, LinearArithExpression exp) {}
	
	public void call(int index, int limit, LinearLogicExpression exp) {
		exp.gen_bool(index, limit, cb);
	}
}