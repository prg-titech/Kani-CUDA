package arrayExpression;

public class TwoCallBack implements CallBack{
	public int depth;
	public CallBack cb;
	
	public TwoCallBack(int depth, CallBack cb) {
		this.depth = depth;
		this.cb = cb;
	}
	public void call(int index, arrExpression exp) {
		exp.gen(index, depth - 1, cb);
	}
}