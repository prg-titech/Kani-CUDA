package Expression;

import org.apache.commons.math3.stat.correlation.PearsonsCorrelation;
import java.io.*;
import java.util.*;


public class PearsonCorrelation {
	public File profile;
	public List<String> vars;
	
	public PearsonCorrelation(File profile, List<String> vars) {
		this.profile = profile;
		this.vars = vars;
	}
	
	public List<String> process() {
		List<String> result = new ArrayList<String>();
		int smidIndex = -1;
		int size = 800;
		for (int i = 0; i < vars.size(); i++) {
			if (vars.get(i).equals("smid")) {
				smidIndex = i;
			}
		}
		if (smidIndex == -1) {
			return vars;
		}
		HashMap<String, double[]> arrMap = new HashMap<>();
		for (int i = smidIndex; i < vars.size(); i++) {
			arrMap.put(vars.get(i), new double[size]);
		}
		try {
			if(profile.exists()) {
				FileReader fr = new FileReader(profile);
				BufferedReader br = new BufferedReader(fr);
				String read;
				int index = 0;
				while ((read = br.readLine()) != null && index < size) {
					List<String> sList = Arrays.asList(read.split(" "));
					if (sList.contains("N") || sList.contains("T") || sList.contains("F")) {
						continue;
					}
					for (int i = smidIndex; i < vars.size(); i++) {
						arrMap.get(vars.get(i))[index] = Integer.parseInt(sList.get(i));
					}
					index++;
				}
				br.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		double[] smidList = arrMap.get(vars.get(smidIndex));
		for (int i = smidIndex + 1; i < vars.size(); i++) {
			double corr = new PearsonsCorrelation().correlation(smidList, arrMap.get(vars.get(i)));
			//System.out.println(corr);
			if (corr > 0.5 || corr < -0.5) {
				result.add(vars.get(i));
				//System.out.println(vars.get(i));
			}
		}
		return result;
	}
}