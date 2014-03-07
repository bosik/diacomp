package org.bosik.diacomp.web.backend.features.analyze.function;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.AnalyzeRec;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.Koof;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.KoofList;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.PrimeRec;

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

	public static List<PrimeRec> extractRecords(DiaryService source, Date fromTime, Date toTime)
	{
		List<PrimeRec> result = new LinkedList<PrimeRec>();
		List<Versioned<DiaryRecord>> recs = source.getRecords(fromTime, toTime, false);

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
		Date mealDate = null;

		Date prevBloodTime = null;
		double prevBloodValue = -1;

		for (Versioned<DiaryRecord> vrec : recs)
		{
			DiaryRecord rec = vrec.getData();
			if (rec instanceof InsRecord)
			{
				curIns = ((InsRecord)rec).getValue();
				ins += curIns;
				if (curIns > maxIns)
				{
					maxIns = curIns;
					timeI = rec.getTime();
				}
			}
			else if (rec instanceof MealRecord)
			{
				MealRecord meal = (MealRecord)rec;
				prots += meal.getProts();
				fats += meal.getFats();
				curCarbs = meal.getCarbs();
				carbs += curCarbs;
				if (curCarbs > maxCarbs)
				{
					maxCarbs = curCarbs;
					timeF = rec.getTime();
					mealDate = rec.getTime();
				}
			}
			else if (rec instanceof BloodRecord)
			{
				// TODO: check postprand property
				BloodRecord blood = (BloodRecord)rec;

				if (prevBloodValue < 0)
				{
					prevBloodTime = rec.getTime();
					prevBloodValue = blood.getValue();
				}
				else if (((carbs > 0) || (prots > 0)) && (ins > 0))
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
					item.setDate(mealDate);

					result.add(item);

					prevBloodTime = blood.getTime();
					prevBloodValue = blood.getValue();
				}
				else
				// there is no meal nor ins before this BS measurement
				{
					prevBloodTime = blood.getTime();
					prevBloodValue = blood.getValue();
				}

				ins = 0.0;
				maxIns = -1.0;
				prots = 0.0;
				fats = 0.0;
				carbs = 0.0;
				maxCarbs = -1.0;
				timeF = null;
				timeI = null;
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
		return ((adaptation - 0.5) * Math.sin(Math.PI * (x - 0.5))) + 0.5;
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

		List<AnalyzeRec> result = new LinkedList<AnalyzeRec>();

		for (PrimeRec rec : recs)
		{
			AnalyzeRec item = new AnalyzeRec();
			item.setProts(rec.getProts());
			item.setFats(rec.getFats());
			item.setCarbs(rec.getCarbs());
			item.setIns(rec.getInsValue());
			item.setBsIn(rec.getBloodInValue());
			item.setBsOut(rec.getBloodOutValue());
			item.setTime(rec.getFoodTime());

			double x = (rec.getDate().getTime() - min) / (curTime - min);
			double w = f(x, adaptation);
			item.setWeight(w);

			result.add(item);
		}

		// normalization

		if (!result.isEmpty())
		{
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
				throw new IllegalArgumentException(String.format("Invalid method: %d", analyzeMethod));
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

	public static KoofList analyze(AnalyzeService analyzer, DiaryService source, Date fromTime, Date toTime,
			double adaptation)
	{
		List<PrimeRec> recs = extractRecords(source, fromTime, toTime);
		List<AnalyzeRec> formatted = formatRecords(recs, adaptation);
		return analyzer.analyze(formatted);
	}
}
