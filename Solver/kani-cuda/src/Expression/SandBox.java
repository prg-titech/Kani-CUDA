package Expression;

import java.io.*;
import java.util.*;

public class SandBox {
		public static boolean convert(int bid, int c, int smid, List<String> lst){
			boolean res = true;
			List<String> datum = new ArrayList<String>();
			for(String e : lst){
				datum = Arrays.asList(e.split(" "));
				if (c == Integer.parseInt(datum.get(2))
				&& bid == Integer.parseInt(datum.get(1))
				&& smid == Integer.parseInt(datum.get(4))){
					return true;
				} else if (c == Integer.parseInt(datum.get(2))
						&& bid == Integer.parseInt(datum.get(1))){
					res = false;
				}
			}
			return res;
		}
		
		public static boolean isReadableThreadAux(int smid, int bid, List<String> lst){
			List<String> datum = new ArrayList<String>();
			for(String e : lst){
				datum = Arrays.asList(e.split(" "));
				if (smid == Integer.parseInt(datum.get(8))
						&& bid == Integer.parseInt(datum.get(1))) {
					return false;
				}
			}
			return true;
		}
		
		public static boolean isReadableThread(int tid, int bid, List<String> data1, List<String> data2){
			boolean res = false;
			List<String> datum = new ArrayList<String>();
			for(String e : data2){
				datum = Arrays.asList(e.split(" "));
				if (tid == Integer.parseInt(datum.get(0)) 
						&& bid == Integer.parseInt(datum.get(1))){
					res = isReadableThreadAux(Integer.parseInt(datum.get(4)), bid, data1);
				}
			}
			return res;
		}
				
	
		public static void main(String[] args){
			try{
				//File file1 = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion3d/profile3");
				//File file = new File("/Users/akira/masuhara-lab/kani-cuda/Kani-CUDA/Examples/Matrixmultiply/profile");
				File file1 = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/himenoBMT/profile");
				File file2 = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion3d/profile3");
				

				long start = System.currentTimeMillis();
				if(file1.exists() && file2.exists()){
					FileReader fr1 = new FileReader(file1);
					FileReader fr2 = new FileReader(file2);
					BufferedReader br1 = new BufferedReader(fr1);
					BufferedReader br2 = new BufferedReader(fr2);
					String datum1 = br1.readLine();
					String datum2 = br2.readLine();
					
					// Save the names of variables
					List<String> vars = Arrays.asList(datum1.split(" "));
					
					Environment env = new Environment(vars);
					
					// LIMIT is 
					int LIMIT = 0;
					
					int size = 0;
					
					System.out.printf("The number of variables: %d%n", vars.size()-2);
					
					// TODO Add datum when env.existsSmIdx() is true.
					List<String> data = new ArrayList<String>();
					
					List<String> data2 = new ArrayList<String>();
					
					while ((datum1 = br1.readLine()) != null) {
						data.add(datum1);
						env.setVal(vars, Arrays.asList(datum1.split(" ")));
						size++;
					}
					fr1.close();
					
					while ((datum2 = br2.readLine()) != null) {
						data2.add(datum2);
					}
					fr2.close();
					
					 
					LIMIT = (int) (size * 0);
					
					
//					loop : for(int k = 0; k<3; k++){
//						List<Expression> exps = env.generateArith2(k);
//		//  					for(Expression e : exps){
//		//  						e.print();
//		//  					}
//						Iterator<Expression> it = exps.iterator();
//						int temp = 0;
//						while(it.hasNext()){
//							Expression e = it.next();
//							temp = 0;
//							for(int i = 0; i < size; i++){
//								env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
//								if (!convert(env.getBid(), env.getGmIdx(), e.eval(env), map)) {
//									temp++;
//								}
//								if (temp > LIMIT){
//									break;
//								}
//								if (i == size-1) {
//									e.print();
//									break loop;
//								}
//							}
//						}
//					}
					
					// System.out.println(LIMIT);
										
					// desired: trow * bdimx + k
/*
					Expression trow = new Variable("trow");
					Expression bdimx = new Variable("bdimx");
					Expression k = new Variable("k");
					Expression exp = trow.multiple(bdimx).plus(k);
					exp.print();
*/
					// Desired expression: (i == 0) ? c2 : c2 - 1
					// (c2 = tidx + tidy * bdimx)

					
//
//					List<String> thns = new ArrayList<String>();
//					List<String> elss = new ArrayList<String>();
//					List<Expression> exps = env.generateArith2(2);
//					Iterator<Expression> it = exps.iterator();
//					// int score = 0;
//					int temp = 0;
//					Expression thn = new Constant<Integer>(0);
//					Expression els = new Constant<Integer>(0);
//					System.out.printf("The number of datum: %d%n", size);
//					loop : while(it.hasNext()){
//						Expression e = it.next();
//						for(int i = 0; i < size; i++){
//							env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
//							if (env.exsitsSmIdx() && (e.eval(env) != env.getSmIdx())) {
//								temp++;
//							}
//							if (temp > LIMIT) {
//								break;
//							}
//							if (i == size - 1) {
//								thn = e;
//								break loop;
//							}
//						}
//						/*
//						if (temp > score) {
//							thn = e;
//							score = temp;
//						}
//						*/
//						temp = 0;
//					}
//					for(int i = 0; i < size; i++){
//						env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
//						if (env.exsitsSmIdx() && (thn.eval(env) == env.getSmIdx())) {
//							thns.add(data.get(i));
//						} else if (env.exsitsSmIdx()) {
//							elss.add(data.get(i));
//						}
//					}
//					//System.out.println(thns.size());
//					//System.out.println(elss.size());
//
//					int sizet = thns.size();
//					int sizee = elss.size();
//					
//					Iterator<Expression> it2 = exps.iterator();
//					
//					loop : while(it2.hasNext()){
//						Expression e = it2.next();
//						for(int i = 0; i < sizee; i++){
//							env.setVal(vars, Arrays.asList(elss.get(i).split(" ")));
//							if (e.eval(env) != env.getSmIdx()) {
//								break;
//							}
//							if (i == sizee - 1) {
//								els = e;
//								break loop;
//							}
//						}
//					}
//					
//					// Bool ::= c | 
//					List<BoolExpression> boolExps = env.generateBool(1);
//					
//					Iterator<BoolExpression> it3 = boolExps.iterator();
//					loop : while(it3.hasNext()){
//						BoolExpression e = it3.next();
//						for (int i = 0; i < sizet; i++){
//							env.setVal(vars, Arrays.asList(thns.get(i).split(" ")));
//							if (!e.eval(env)) {
//								break;
//							}
//							if (i == sizet - 1) {
//								for (int j = 0; j < sizee; j++) {
//									env.setVal(vars, Arrays.asList(elss.get(j).split(" ")));
//									if (e.eval(env)) {
//										break;
//									}
//									if (j == sizee - 1) {
//										System.out.println("Synthesized expression:");
//										new IfExpression(e, thn, els).print();
//										break loop;
//									}
//									if (j == sizee - 1) {
//										System.out.println("Unsat");
//									}
//								}
//							}
//						}
//					}
//


/*
					// then: (c - (c - c2))
					// else: (tid-x + ((tid-y * bx) - 1))
 
					List<String> thns = new ArrayList<String>();
					List<String> elss = new ArrayList<String>();
					List<Expression> exps = env.generateArith2(2);
					int sizeExps = exps.size();
					Expression thn = new Constant<Integer>(0);
					Expression els = new Constant<Integer>(0);
					System.out.println(size);
					loop : for (int i = 0; i < sizeExps - 1; i++) {
						thn = exps.get(i);
						for (int j = i + 1; j < sizeExps; j++) {
							els = exps.get(j);
							for (int k = 0; k < size; k++){
								env.setVal(vars, Arrays.asList(data.get(k).split(" ")));
								if (env.exsitsSmIdx() && thn.eval(env) != env.getSmIdx() && els.eval(env) != env.getSmIdx()) {
									break;
								}
								if (k == size - 1) {
									break loop;
								}
							}
						}
					}
					thn.print();
					els.print();
					
					List<BoolExpression> boolExps = env.generateBool(1);
					
					Iterator<BoolExpression> it = boolExps.iterator();
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
							
					

/*					
 					// Synthesize arithmetic expression
  					List<Expression> exps = env.generateArith2(2);
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
*/
		

//					// Synthesize boolean expression 
//					List<BoolExpression> exps = env.generateBool(1);
//					
//					Iterator<BoolExpression> it = exps.iterator();
//					loop : while(it.hasNext()){
//						BoolExpression e = it.next();
//						boolean temp = false;
//						for(int i = 0; i < size; i++){
//							env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
//							temp = env.exsitsSmIdx();
//							if (e.eval(env) != temp) {
//								break;
//							}
//							if (i == size-1) {
//								e.print();
//								break loop;
//							}
//						}
//					}
					
					// Synthesize boolean expression(sleeve ver.)
//					List<BoolExpression> exps = env.generateBool(1);
//					
//					Iterator<BoolExpression> it = exps.iterator();
//					loop : while(it.hasNext()){
//						BoolExpression e = it.next();
//						boolean temp = false;
//						for(int i = 0; i < size; i++){
//							env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
//							temp = isReadableThread(env.getTid(), env.getBid(), data, data2);
//							if (e.eval(env) != temp) {
//								break;
//							}
//							if (i == size-1) {
//								e.print();
//								break loop;
//							}
//						}
//					}
					
					List<BoolExpression> exps = env.generateBool(1);
					
					Iterator<BoolExpression> it = exps.iterator();
					loop : while(it.hasNext()){
						BoolExpression e = it.next();
						boolean temp = false;
						for(int i = 0; i < size; i++){
							env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
							temp = (env.getSmIdx() == 0);
							if (e.eval(env) != temp) {
								break;
							}
							if (i == size-1) {
								e.print();
								break loop;
							}
						}
					}

					long end = System.currentTimeMillis();
					System.out.println("time: " + (end - start) + "ms");
				}
			} catch (IOException e) {
				e.printStackTrace();
			}	
		}	
}
