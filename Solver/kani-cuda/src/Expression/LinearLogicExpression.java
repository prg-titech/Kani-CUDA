package Expression;

import java.util.*;

/*
 * 0 : pass		-4 : !=
 * -1 : &&		-5 : <
 * -2 : ||		-6 : >
 * -3 : ==
 */

public class LinearLogicExpression {
	
	public int[] arr;
	private int[] consts;
	private int[] vars;
	private int[][] profile;
	private int cSize;
	private int vSize;
	private int M;
	public ArrayList<int[]> lst;
	private int smidIndex;
	private int lineCount;
	private List<String> varNames;
	
	int count = 0;
	
	public LinearLogicExpression(int[] consts, int[] vars,
			int[][] profile, int smidIndex, int lineCount, List<String> names) {
		this.consts = consts;
		this.vars = vars;
		this.profile = profile;
		this.smidIndex = smidIndex;
		this.lineCount = lineCount;
		this.varNames = names;
		cSize = consts.length;
		vSize = vars.length;
		M = cSize + vSize + 1;
		lst = new ArrayList<>();
	}
	
	public String generate() {
		try {
			gen();
		} catch (Exception e) {
			return e.getMessage();
		}
		return "f";
	}
	
	public void gen() {
		arr = new int[25];
		gen_arith();
		TestCallBack tcb = new TestCallBack();
		gen_bool(0, Integer.MAX_VALUE, tcb);
		arr[0] = -1;
		gen_bool(1, Integer.MAX_VALUE, new LogicCallBack(tcb));
		arr[0] = -2;
		gen_bool(1, Integer.MAX_VALUE, new LogicCallBack(tcb));
		//System.out.println(count + " logic expressions are correct");
		//System.out.println(tcb.count_logic);
	}
	
	public void gen_bool(int index, int limit, CallBack cb) {
		int curr = 0;
		for (int i = 0; i < lst.size(); i++) {
			for (int j = 0; j < i; j++) {
				curr = (i + 1) * (lst.size()) + (j + 1);
				if (curr >= limit) { return; }
				int[] left = lst.get(i);
				int[] right = lst.get(j);
				System.arraycopy(left, 0, arr, index + 1, left.length);
				System.arraycopy(right, 0, arr, index + 1 + left.length, right.length);
				for (int k = -3; k >= -6; k--) {
					arr[index] = k;
					cb.call(index + left.length + right.length + 1, curr, this);
				}
			}
		}
	}
	
	public void gen_arith() {
		int[] zero = new int[2];
		zero[0] = 1;
		zero[1] = 0;
		lst.add(zero);
		
		for (int i = 0; i < vSize; i++) {
			int[] curr = new int[2];
			curr[0] = 2;
			curr[1] = vars[i];
			lst.add(curr);
		}
		
		for (int i = 0; i < vSize; i++) {
			for (int j = 0; j < cSize; j++) {
				int[] plus = new int[5];
				plus[0] = 3;
				plus[1] = 2;
				plus[2] = vars[i];
				
				int[] minus = new int[5];
				minus[0] = 4;
				minus[1] = 2;
				minus[2] = vars[i];
				
				plus[3] = 1;
				plus[4] = consts[j];
				lst.add(plus);
				minus[3] = 1;
				minus[4] = consts[j];
				lst.add(minus);
			}
			
		}
	}
	
	public int[] getExpression() {
		return arr;
	}
	
	public boolean test(int index) {
		int x;
		Cursor cursor = new Cursor();
		boolean curr;
		for (int line = 0; line < lineCount; line++) {
			cursor.setIndex(0);
			curr = evaluate(cursor, line);
			if ((x = profile[line][smidIndex]) == -1) {
				if (curr) {
					return false;
				}
			} else {
				if (!curr) {
					return false;
				}
			}
		}
		count++;
		throw new RuntimeException(logicToString());
	}
	
	public boolean evaluate(Cursor cursor, int line) {
		boolean result;
		int index = cursor.getIndex();
		cursor.addIndex(1);
		if (arr[index] == -1) {
			result = evaluate(cursor, line);
			if (result == false) { return false; }
			return result && evaluate(cursor, line);
		} else if (arr[index] == -2) {
			result = evaluate(cursor, line);
			if (result == true) { return true; }
			return result || evaluate(cursor, line);
		} else {
			int left = 0;
			left = eval_arith(cursor, line);
			switch(arr[index]) {
			case -3: return left == eval_arith(cursor, line);
			case -4: return left != eval_arith(cursor, line);
			case -5: return left < eval_arith(cursor, line);
			case -6: return left > eval_arith(cursor, line);
			}
		}
		return false;
	}
	
	public int eval_arith(Cursor cursor, int line) {
		int index = cursor.getIndex();
		int result;
		switch(arr[index]) {
		case 1: cursor.addIndex(2);
				return arr[index + 1];
		case 2: cursor.addIndex(2);
				return profile[line][arr[index + 1]];
		case 3: cursor.addIndex(1);
				result = eval_arith(cursor, line);
				return result + eval_arith(cursor, line);
		case 4: cursor.addIndex(1);
				result = eval_arith(cursor, line);
				return result - eval_arith(cursor, line);
		}
		return 0;
	}
	
	public String logicToString() {
		return helper(new Cursor());
	}
 	
	public String helper(Cursor cursor) {
		String result = " ";
		int index = cursor.getIndex();
		cursor.addIndex(1);
		switch (arr[index]) {
		case -1:
			result = "( " + helper(cursor);
			return result += "&& " + helper(cursor) + ") ";
		case -2:
			result = "( " + helper(cursor);
			return result += "|| " + helper(cursor) + ") ";
		case -3:
			result = "( " + helper(cursor);
			return result += "== " + helper(cursor) + ") ";
		case -4:
			result = "( " + helper(cursor);
			return result += "!= " + helper(cursor) + ") ";
		case -5:
			result = "( " + helper(cursor);
			return result += "< " + helper(cursor) + ") ";
		case -6:
			result = "( " + helper(cursor);
			return result += "> " + helper(cursor) + ") ";
		case 1:
			cursor.addIndex(1);
			return result += arr[index + 1] + " ";
		case 2:
			cursor.addIndex(1);
			return result += varNames.get(arr[index + 1]) + " ";
		case 3:
			result = "( " + helper(cursor);
			return result += "+ " + helper(cursor) + ") ";
		case 4:
			result = "( " + helper(cursor);
			return result += "- " + helper(cursor) + ") ";
		}
		return result;
	}
}