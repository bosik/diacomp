/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.analyze;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.services.analyze.entities.PrimeRec;
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

public class AnalyzeExtracter
{
	public enum ValueFunction {
		LINEAR_ABS, LINEAR_AVG, DISTANCE, QUADRIC
	}

	private static int extractMin(Date date)
	{
		long val = date.getTime() / Utils.MsecPerMin;
		return (int)val;
	}

	public static List<PrimeRec> extractPrimeRecords(List<Versioned<DiaryRecord>> recs)
	{
		List<PrimeRec> result = new LinkedList<PrimeRec>();

		PostprandUtils.updatePostprand(recs);

		double ins = 0.0;
		double curIns = 0.0;
		double maxIns = -1.0;
		double prots = 0.0;
		double fats = 0.0;
		double carbs = 0.0;
		double curCarbs;
		double maxCarbs = -1.0;
		Date timeF = null;
		Date timeI = null;

		final long MAX_BLOCK_TIME = 12 * Utils.MsecPerHour;

		Date prevBloodTime = null;
		double prevBloodValue = -1;

		for (Versioned<DiaryRecord> versionedRecord : recs)
		{
			DiaryRecord record = versionedRecord.getData();
			if (record instanceof InsRecord)
			{
				// System.out.print(versionedRecord.getId() + "\tins\t");
				curIns = ((InsRecord)record).getValue();
				ins += curIns;
				if (curIns > maxIns)
				{
					maxIns = curIns;
					timeI = record.getTime();
					// System.out.println("max updated");
				}
				else
				{
					// System.out.println("");
				}
			}
			else if (record instanceof MealRecord)
			{
				// System.out.print(versionedRecord.getId() + "\tmeal\t");
				MealRecord meal = (MealRecord)record;
				prots += meal.getProts();
				fats += meal.getFats();
				curCarbs = meal.getCarbs();
				carbs += curCarbs;
				if (curCarbs > maxCarbs)
				{
					maxCarbs = curCarbs;
					timeF = record.getTime();
					// System.out.println("max updated");
				}
				else
				{
					// System.out.println("");
				}
			}
			else if (record instanceof BloodRecord)
			{
				BloodRecord blood = (BloodRecord)record;
				// System.out.print(versionedRecord.getId() + "\tblood\t");

				if (!blood.isPostPrand())
				{
					if ((prevBloodValue > 0) && ((carbs > 0) || (prots > 0)) && (ins > 0) && (prevBloodTime != null)
							&& (record.getTime().getTime() - prevBloodTime.getTime() < MAX_BLOCK_TIME))
					{
						PrimeRec item = new PrimeRec();
						item.setBloodInTime(extractMin(prevBloodTime));
						item.setBloodInValue(prevBloodValue);
						item.setInsTime(extractMin(timeI));
						item.setInsValue(ins);
						item.setFoodTime(extractMin(timeF));
						item.setProts(prots);
						item.setFats(fats);
						item.setCarbs(carbs);
						item.setBloodOutTime(extractMin(blood.getTime()));
						item.setBloodOutValue(blood.getValue());
						item.setDate(timeF);
						result.add(item);
						// System.out.println(String.format("OK - new item added\t%.1f\t%.1f\t%.1f\t%.1f",
						// 		item.getBloodInValue(), item.getInsValue(), item.getCarbs(), item.getBloodOutValue()));
					}
					else
					{
						// System.out.println("conditions failed - ignored");
					}

					prevBloodTime = blood.getTime();
					prevBloodValue = blood.getValue();

					ins = 0.0;
					maxIns = -1.0;
					prots = 0.0;
					fats = 0.0;
					carbs = 0.0;
					maxCarbs = -1.0;
					timeF = null;
					timeI = null;
				}
				else
				{
					// System.out.println("postprandial - ignored");
				}
			}
		}

		// restoring day minutes

		for (PrimeRec rec : result)
		{
			int timeShift;
			if ((rec.getProts() > 0) || (rec.getCarbs() > 0))
			{
				timeShift = rec.getFoodTime() / Utils.MinPerDay;
			}
			else if (rec.getInsValue() > 0)
			{
				timeShift = rec.getInsTime() / Utils.MinPerDay;
			}
			else
			{
				timeShift = (rec.getBloodInTime() + rec.getBloodOutTime()) / 2;
			}

			timeShift *= Utils.MinPerDay;

			rec.setBloodInTime(rec.getBloodInTime() - timeShift);
			rec.setBloodOutTime(rec.getBloodOutTime() - timeShift);
			if (rec.getInsTime() > -1)
			{
				rec.setInsTime(rec.getInsTime() - timeShift);
			}
			if (rec.getFoodTime() > -1)
			{
				rec.setFoodTime(rec.getFoodTime() - timeShift);
			}
		}

		return result;
	}

	private static double f(double x, double adaptation)
	{
		//return (adaptation - 0.5d) * Math.sin(Math.PI * (x - 0.5d)) + 0.5d;
		return 1.0 + 0.5 * adaptation * (Math.sin(Math.PI * (x - 0.5)) - 1);
	}

	/**
	 * 
	 * @param recs
	 * @param adaptation
	 *            in [0..0.5]: 0.0 is the quickest (but unstable), 0.5 is the slowest (but very stable)
	 * @return
	 */
	public static List<AnalyzeRec> formatRecords(List<PrimeRec> recs, double adaptation)
	{
		List<AnalyzeRec> result = new ArrayList<AnalyzeRec>();

		if (!recs.isEmpty())
		{
			// calculating min

			long curTime = new Date().getTime();
			long min = curTime;

			for (PrimeRec rec : recs)
			{
				if (rec.getDate().getTime() < min)
				{
					min = rec.getDate().getTime();
				}
			}

			// building

			for (PrimeRec rec : recs)
			{
				AnalyzeRec item = new AnalyzeRec();
				item.setProts(rec.getProts());
				item.setFats(rec.getFats());
				item.setCarbs(rec.getCarbs());
				item.setIns(rec.getInsValue());
				item.setBsIn(rec.getBloodInValue());
				item.setBsOut(rec.getBloodOutValue());
				// FIXME: hardcoded time zone
				item.setTime((rec.getFoodTime() + 3 * 60) % Utils.MinPerDay); // FIXME

				double x = (double)(rec.getDate().getTime() - min) / (curTime - min);
				double w = f(x, adaptation);
				//System.out.println(String.format("f(%.2f, %.2f) = %.4f", x, adaptation, w));
				item.setWeight(w);

				result.add(item);
			}

			// normalization

			double minW = result.get(0).getWeight();
			double maxW = result.get(0).getWeight();

			for (AnalyzeRec rec : result)
			{
				minW = Math.min(minW, rec.getWeight());
				maxW = Math.max(maxW, rec.getWeight());
			}

			if ((maxW - minW) > Utils.EPS)
			{
				for (AnalyzeRec rec : result)
				{
					rec.setWeight((rec.getWeight() - minW) / (maxW - minW));
				}
			}
			else
			{
				for (AnalyzeRec rec : result)
				{
					rec.setWeight(1.0);
				}
			}
		}

		return result;
	}

	public static double getRecError(AnalyzeRec rec, KoofList koofs, ValueFunction analyzeMethod)
	{
		Koof koof = koofs.getKoof(rec.getTime());
		double err = (rec.getBsIn() + (rec.getCarbs() * koof.getK()) + (rec.getProts() * koof.getP()))
				- (rec.getIns() * koof.getQ()) - rec.getBsOut();

		switch (analyzeMethod)
		{
			case LINEAR_ABS:
				return Math.abs(err);
			case LINEAR_AVG:
				return err;
			case DISTANCE:
			{
				double carbs2 = rec.getCarbs() * rec.getCarbs();
				double prots2 = rec.getProts() * rec.getProts();
				double ins2 = rec.getIns() * rec.getIns();
				return Math.abs(err) / Math.sqrt(carbs2 + prots2 + ins2);
			}
			case QUADRIC:
				return err * err;
			default:
				throw new IllegalArgumentException(String.format("Invalid method: %s", analyzeMethod.toString()));
		}
	}

	public static double getRecListError(List<AnalyzeRec> recs, KoofList koofs, ValueFunction analyzeMethod)
	{
		double result = 0.0;
		for (AnalyzeRec rec : recs)
		{
			result += getRecError(rec, koofs, analyzeMethod);
		}

		if (!recs.isEmpty())
		{
			result /= recs.size();
		}

		if (analyzeMethod == ValueFunction.QUADRIC)
		{
			result = Math.sqrt(result);
		}

		return result;
	}
}
