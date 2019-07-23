package Expression;

import org.apache.commons.math3.stat.correlation.PearsonsCorrelation;
import org.apache.commons.math3.stat.descriptive.moment.Variance;
import java.util.*;

/*
 * For quick search
 * Use statistical techniques to eliminate unrelated variables
 * 
 * pearsoncorrelation (-1.0 ~ 1.0):
 * 		tests the correlation between two variables
 * 		0 : no correlation
 * 		-0.3 ~ 0.3 : weak correlation
 * 		-0.6 ~ -0.3 / 0.3 ~ 0.6 : fair correlation
 * 		-1.0 ~ -0.6 / 0.6 ~ 1.0 : strong correlation
 * 		discard variables with weak correlation to shared memory index
 * 
 * constant variable:
 * 		variable that remains constant in one profile
 * 		always fails pearcorrelation test, but might be included in the expression
 * 		variance = 0 --> constant variable
 */

public class PearsonCorrelation {
	private List<String> vars;
	private int[] arr;
	private int[][] avail_arr;
	private Map<String, double[]> arrMap;
	private int smidIndex;
	int count = 0;
	private int avail_line_count;
	
	public PearsonCorrelation(List<String> vars, int[][] avail, int avail_line_count) {
		this.vars = vars;
		this.arrMap = new HashMap<>();
		this.smidIndex = -1;
		this.avail_arr = avail;
		this.avail_line_count = avail_line_count;
		arr = new int[vars.size()];
	}
	
	public void process() {
		// hard coded this size
		int size = 800;
		for (int i = 0; i < vars.size(); i++) {
			if (vars.get(i).equals("smid")) {
				this.smidIndex = i;
			}
		}
		if (smidIndex == -1) {
			return;
		}
		for (int i = smidIndex; i < vars.size(); i++) {
			arrMap.put(vars.get(i), new double[size]);
		}
		for (int i = 0; i < avail_line_count; i++) {
			int index = 0;
			for (int j = smidIndex; j < vars.size(); j++) {
				arrMap.get(vars.get(j))[index] = avail_arr[i][j];
			}
			index++;
		}
	}
	
	public void getRelVars() {
		double[] smidList = arrMap.get(vars.get(smidIndex));
		for (int i = smidIndex + 1; i < vars.size(); i++) {
			double corr = new PearsonsCorrelation().correlation(smidList, arrMap.get(vars.get(i)));
			if (corr > 0.5 || corr < -0.5) {
				arr[count++] = i;
			}
		}
	}
	
	public void getConstants() {
		Variance v = new Variance();
		for (int i = smidIndex + 1; i < vars.size(); i++) {
			if (v.evaluate(arrMap.get(vars.get(i))) == 0) {
				arr[count++] = i;
			}
		}
	}
	
	public int[] getVarIndex() {
		getRelVars();
		getConstants();
		int[] varIndex = new int[count];
		System.arraycopy(arr, 0, varIndex, 0, count);
		return varIndex;
	}
	
	public int getSmid() {
		return smidIndex;
	}
}