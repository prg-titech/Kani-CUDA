package arrayExpression;

public class BooleanTwoCallBack implements CallBack {
	public int depth;
	public CallBack cb;
	
	public BooleanTwoCallBack(int depth, CallBack cb) {
		this.depth = depth;
		this.cb = cb;
	}
	
	public void call(int index, arrExpression exp) {
		exp.generate1(index, depth, cb);
	}
}