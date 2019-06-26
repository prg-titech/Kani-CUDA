package arrayExpression;

import java.util.Arrays;

public class arrExpression {
	private int varNum;
	private int[] arr;
	private int[][] vars;
	private int branchNum = 3;
	
	public arrExpression(int varNum) {
		this.varNum = varNum;
	}
	
	public void generate(int depth) {
		if (depth == 0) { return; }
		arr = new int[3 * (int) Math.pow(2, depth - 1) - 1];
		TestCallBack tcb = new TestCallBack();
		gen(0, depth, tcb);
		System.out.println(tcb.count);
	}
	
	public int[] getExpression() {
		return this.arr;
	}
	
	public void gen(int index, int depth, CallBack cb) {
		if (depth == 0) { return; }
		gen_const(index, cb);
		gen_var(index, cb);
		gen_add(index, depth, cb);
		gen_minus(index, depth, cb);
		gen_multiply(index, depth, cb);
	}
	
	public void gen_const(int index, CallBack cb) {
		arr[index] = 1;
		for (int i = 1; i < 3; i++) {
			arr[index + 1] = i;
			cb.call(index + 2, this);
			//System.out.println(Arrays.toString(arr));
		}
	}
	
	public void gen_var(int index, CallBack cb) {
		arr[index] = 2;
		for (int i = 0; i < varNum; i++) {
			arr[index + 1] = i;
			cb.call(index + 2, this);
			//System.out.print(Arrays.toString(arr));
		}
	}
	
	public void gen_add(int index, int depth, CallBack cb) {
		arr[index] = 3;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}
	
	public void gen_minus(int index, int depth, CallBack cb) {
		arr[index] = 4;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}
	
	public void gen_multiply(int index, int depth, CallBack cb) {
		arr[index] = 5;
		CallBack tcb = new TwoCallBack(depth, cb);
		gen(index + 1, depth - 1, tcb);
	}
}