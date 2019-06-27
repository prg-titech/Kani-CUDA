package arrayExpression;

public class arrBoolExpression implements arrExpression {
	private int[] arr;
	private int varNum;
	
	public arrBoolExpression(int varNum) {
		arr = new int[25];
		this.varNum = varNum;
	}
	
	public void generate(int depth) {
		if (depth == 0) { return; }
		TestCallBack tcb = new TestCallBack();
		//CallBack bcb = new BooleanCallBack(depth, tcb);
		gen_pass(0, depth, tcb);
		gen_and(0, depth, new BooleanTwoCallBack(depth, tcb));
		gen_or(0, depth, new BooleanTwoCallBack(depth, tcb));
		System.out.println(tcb.count);
	}
	
	public int[] getExpression() {
		return arr;
	}
	
	public void generate1(int index, int depth, CallBack cb) {
		gen_equals(index, depth, cb);
		gen_not_equals(index, depth, cb);
		gen_greater(index, depth, cb);
	}
	
	public void gen(int index, int depth, CallBack cb) {
		if (depth == 0) { return; }
		gen_const(index, cb);
		gen_var(index, cb);
		gen_add(index, depth, cb);
	}
	
	public void gen_pass(int index, int depth, CallBack cb) {
		arr[index] = 0;
		generate1(index + 1, depth, cb);
	}
	
	public void gen_and(int index, int depth, CallBack cb) {
		arr[index] = -1;
		generate1(index + 1, depth, cb);
	}
	
	public void gen_or(int index, int depth, CallBack cb) {
		arr[index] = -2;
		generate1(index + 1, depth, cb);
	}
	
	public void gen_equals(int index, int depth, CallBack cb) {
		arr[index] = -3;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}

	public void gen_not_equals(int index, int depth, CallBack cb) {
		arr[index] = -4;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}
	
	public void gen_greater(int index, int depth, CallBack cb) {
		arr[index] = -5;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}
	
	public void gen_const(int index, CallBack cb) {
		arr[index] = 1;
		for (int i = 1; i < 2; i++) {
			arr[index + 1] = i;
			cb.call(index + 2, this);
		}
	}
	
	public void gen_var(int index, CallBack cb) {
		arr[index] = 2;
		for (int i = 0; i < varNum; i++) {
			arr[index + 1] = i;
			cb.call(index + 2, this);
		}
	}
	
	public void gen_add(int index, int depth, CallBack cb) {
		arr[index] = 3;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}
	
	public int evaluate(Cursor cursor) {
		return 0;
	}
}