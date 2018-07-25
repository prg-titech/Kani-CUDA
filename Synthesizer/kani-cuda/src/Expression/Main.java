package Expression;

import java.io.*;
import java.util.*;

public class Main {
		public static void main(String[] args){
			try{
				//File file = new File("/Users/akira/masuhara-lab/kani-cuda/Kani-CUDA/Examples/convolution/profile");
				File file = new File("/Users/akira/masuhara-lab/kani-cuda/Kani-CUDA/Examples/Matrixmultiply/profile");
				//File file = new File("/Users/akira/masuhara-lab/kani-cuda/Kani-CUDA/Examples/Diffusion3d/profile");
				
				long start = System.currentTimeMillis();
				if(file.exists()){
					FileReader fr = new FileReader(file);
					BufferedReader br = new BufferedReader(fr);
					String datum = br.readLine();
					
					// Save the names of variables
					List<String> vars = Arrays.asList(datum.split(" "));
					System.out.println(vars.size());
					
					List<String> data = new ArrayList<String>();
					while ((datum = br.readLine()) != null) {
						data.add(datum);
					}
					fr.close();
					
					int size = data.size();
					
					Environment env = new Environment(vars);
					
					//desired: trow * bdimx + k
/*
					Expression trow = new Variable("trow");
					Expression bdimx = new Variable("bdimx");
					Expression k = new Variable("k");
					Expression exp = trow.multiple(bdimx).plus(k);
					exp.print();
*/
					


/*
					while ((datum = br.readLine()) != null) {
						env.setVal(vars, Arrays.asList(datum.split(" ")));
						int smidx = env.getSmIdx();
						Iterator<Expression> it = exps.iterator();
						while(it.hasNext()){
							Expression e = it.next();
							if(e.eval(env) != smidx){
								it.remove();
							}
						}
					}
					exps.get(0).print();
					fr.close();
*/
				
  					List<Expression> exps = env.generateArith2(3);
					Iterator<Expression> it = exps.iterator();
					loop : while(it.hasNext()){
						Expression e = it.next();
						for(int i = 0; i < size; i++){
							env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
							if (env.exsitsSmIdx() && (e.eval(env) != env.getSmIdx())) {
								break;
							}
							if (i == size-1) {
								e.print();
								break loop;
							}
						}
					}
		
/*
					
					List<BoolExpression> exps = env.generateBool(1);
					
					Iterator<BoolExpression> it = exps.iterator();
					loop : while(it.hasNext()){
						BoolExpression e = it.next();
						boolean temp = false;
						for(int i = 0; i < size; i++){
							env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
							temp = env.exsitsSmIdx();
							if (e.eval(env) != temp) {
								break;
							}
							if (i == size-1) {
								e.print();
								break loop;
							}
						}
					}
*/
					long end = System.currentTimeMillis();
					System.out.println("time: " + (end - start) + "ms");
				}
			} catch (IOException e) {
				e.printStackTrace();
			}	
		}	
}
