package arrayExpression;

public interface arrExpression {
	public void generate(int depth);
	public void gen(int index, int depth, CallBack cb);
	public int[] getExpression();
	public int evaluate(Cursor cursor);
	public void generate1(int index, int depth, CallBack cb);
}