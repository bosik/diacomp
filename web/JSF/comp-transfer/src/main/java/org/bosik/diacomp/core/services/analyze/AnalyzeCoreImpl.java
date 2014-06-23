package org.bosik.diacomp.core.services.analyze;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.services.analyze.entities.PrimeRec;
import org.bosik.diacomp.core.services.analyze.entities.WeightedTimePoint;
import org.bosik.diacomp.core.utils.Utils;

public class AnalyzeCoreImpl implements AnalyzeCore
{
	public AnalyzeCoreImpl(double approxFactor)
	{
		// pre-calculation
		for (int i = 0; i < TIME_WEIGHTS.length; i++)
		{
			TIME_WEIGHTS[i] = Math.exp(-approxFactor * Math.pow(i / Utils.HalfMinPerDay, 2));
		}
	}

	private class Bean
	{
		public double	p;
		public double	q;
		public double	g[]	= new double[3];

		public Bean()
		{

		}
	}

	private interface DevFunction
	{
		double calculate(double x, double y);
	}

	class AbsDev implements DevFunction
	{
		@Override
		public double calculate(double x, double y)
		{
			return Math.abs(x - y);
		}
	}

	class SqrDev implements DevFunction
	{
		@Override
		public double calculate(double x, double y)
		{
			return Math.pow(x - y, 2);
		}
	}

	class RelDev implements DevFunction
	{
		@Override
		public double calculate(double x, double y)
		{
			return Math.abs(1 - (x / y));
		}
	}

	private final double	TIME_WEIGHTS[]	= new double[Utils.HalfMinPerDay + 1];

	/**
	 * O(1)
	 * 
	 * @param rec
	 * @param q
	 * @param p
	 * @return
	 */
	private static double calculateK(AnalyzeRec rec, double q, double p)
	{
		if (Math.abs(rec.getCarbs()) > Utils.EPS)
		{
			return (((rec.getBsOut() - rec.getBsIn()) + (rec.getIns() * q)) - (rec.getProts() * p)) / rec.getCarbs();
		}
		else
		{
			return Double.NaN;
		}
	}

	/**
	 * O(1)
	 * 
	 * @param rec
	 * @return
	 */
	private static Double calculateW(AnalyzeRec rec)
	{
		return rec.getWeight();
	}

	/**
	 * O(N) (~50)
	 * 
	 * @param recs
	 * @param q
	 * @param p
	 * @return
	 */
	private static WeightedTimePoint[] calculateKW(List<AnalyzeRec> recs, double q, double p)
	{
		WeightedTimePoint[] result = new WeightedTimePoint[recs.size()];

		for (int i = 0; i < result.length; i++)
		{
			result[i] = new WeightedTimePoint();
			result[i].setTime(recs.get(i).getTime());
			result[i].setValue(calculateK(recs.get(i), q, p));
			result[i].setWeight(calculateW(recs.get(i)));
		}

		return result;
	}

	/**
	 * O(1)
	 * 
	 * @param time1
	 * @param time2
	 * @return
	 */
	private static int timeDistance(int time1, int time2)
	{
		int result = Math.abs(time1 - time2) % Utils.MinPerDay;
		if (result > Utils.HalfMinPerDay)
		{
			result = Utils.MinPerDay - result;
		}
		return result;
	}

	/**
	 * O(1)
	 * 
	 * @param time1
	 * @param time2
	 * @return
	 */
	private double timeWeight(int time1, int time2)
	{
		int dist = timeDistance(time1, time2);
		System.out.println("distance " + time1 + " & " + time2 + " is " + dist);
		return TIME_WEIGHTS[dist];
	}

	/**
	 * O(N) (~50)
	 * 
	 * @param points
	 * @param time
	 * @return
	 */
	private double approximatePoint(WeightedTimePoint[] points, int time)
	{
		double summ = 0.0;
		double summWeight = 0.0;
		for (int i = 0; i < points.length; i++)
		{
			if (!Double.isNaN(points[i].getValue()))
			{
				// TODO: optimize
				double curWeight = points[i].getWeight() * timeWeight(time, points[i].getTime());
				summWeight += curWeight;
				summ += points[i].getValue() * curWeight;
			}
		}

		if (Math.abs(summWeight) < Utils.EPS)
		{
			return Double.NaN;
		}
		else
		{
			return summ / summWeight;
		}
	}

	/**
	 * O(N * M) (~72000) (~1200)
	 * 
	 * @param points
	 * @return
	 */
	private double[] approximate(WeightedTimePoint[] points, boolean highResolution)
	{
		double[] result = new double[Utils.MinPerDay];

		// TODO: improve handling
		if (points.length == 0)
		{
			for (int i = 0; i < Utils.MinPerDay; i++)
			{
				result[i] = Double.NaN;
			}
		}
		else
		{
			if (highResolution)
			{
				for (int i = 0; i < Utils.MinPerDay; i++)
				{
					result[i] = approximatePoint(points, i);
				}
			}
			else
			{
				for (int i = 0; i < Utils.MinPerDay / Utils.MinPerHour; i++)
				{
					result[i * Utils.MinPerHour] = approximatePoint(points, i * Utils.MinPerHour);
				}

				// linear interpolation
				for (int left = 0; left < Utils.MinPerDay; left += Utils.MinPerHour) //0, 60, 120, ..., 1380
				{
					int right = (left + Utils.MinPerHour) % Utils.MinPerDay;

					for (int m = 1; m < Utils.MinPerHour; m++) // 1..59
					{
						double w = m / Utils.MinPerHour;
						result[left + m] = (1 - w) * result[left] + w * result[right];
					}
				}
			}
		}

		return result;
	}

	/**
	 * O(N) (~50)
	 * 
	 * @param recs
	 * @param points
	 * @param func
	 * @return
	 */
	private static double getRand(double[] recs, WeightedTimePoint[] points, DevFunction func)
	{
		double result = 0.0;
		int n = 0;
		for (int i = 0; i < points.length; i++)
			if (!Double.isNaN(points[i].getValue()))
			{
				result += func.calculate(recs[points[i].getTime()], points[i].getValue());
				n++;
			}

		if (n > 0)
		{
			result /= n;
		}

		return result;
	}

	/**
	 * O(N) (~50)
	 * 
	 * @param recs
	 * @param koofs
	 * @param func
	 * @return
	 */
	private static double getDev(List<AnalyzeRec> recs, KoofList koofs, DevFunction func)
	{
		double result = 0.0;
		int n = 0;
		for (AnalyzeRec rec : recs)
		{
			result += func.calculate(
					((rec.getBsIn() + (koofs.getKoof(rec.getTime()).getK() * rec.getCarbs())) - (koofs.getKoof(
							rec.getTime()).getQ() * rec.getIns()))
							+ (koofs.getKoof(rec.getTime()).getP() * rec.getProts()), rec.getBsOut());
			n++;
		}

		if (n > 0)
		{
			result /= n;
		}

		return result;
	}

	/**
	 * O(M) (~1440) (~24)
	 * 
	 * @param ks
	 * @param q
	 * @param p
	 * @return
	 */
	private static KoofList copyKQP(double[] ks, double q, double p)
	{
		KoofList result = new KoofList();
		for (int i = 0; i < Utils.MinPerDay; i++)
		{
			result.getKoof(i).setK(ks[i]);
			result.getKoof(i).setQ(q);
			result.getKoof(i).setP(p);
		}
		return result;
	}

	@Override
	public KoofList analyze(List<Versioned<DiaryRecord>> records)
	{
		double adaptation = 0.95;

		List<PrimeRec> prime = AnalyzeExtracter.extractPrimeRecords(records);
		List<AnalyzeRec> items = AnalyzeExtracter.formatRecords(prime, adaptation);

		for (AnalyzeRec item : items)
		{
			System.out.println(String.format("%d\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f", item.getTime(), item.getWeight(),
					item.getProts(), item.getFats(), item.getCarbs(), item.getIns(), item.getBsOut() - item.getBsIn()));
		}

		/**
		 * This method assumes the Q and P koofs are fixed and K is floating within the day
		 */

		if (items.isEmpty())
		{
			//throw new IllegalArgumentException("Recs list is empty");
			return null;
		}

		final double DISC_Q = 0.0125;
		final double MIN_Q = 1.50;
		final double MAX_Q = 5.00 + (DISC_Q / 2);

		final double DISC_P = 0.05;
		final double MIN_P = 0.00;
		final double MAX_P = 0.00 + (DISC_P / 2);

		KoofList koofs = new KoofList();
		WeightedTimePoint[] points;
		List<Bean> V = new ArrayList<Bean>();
		double k[];
		DevFunction funcRelative = new RelDev();
		DevFunction funcSqr = new SqrDev();

		// 20 605 200 (384 720)
		for (double q = MIN_Q; q < MAX_Q; q += DISC_Q)
		{
			for (double p = MIN_P; p < MAX_P; p += DISC_P) // *280
			{
				Bean bean = new Bean();
				bean.q = q;
				bean.p = p;

				points = calculateKW(items, q, p); // 50
				k = approximate(points, false); // 72 000 (1 200)
				koofs = copyKQP(k, q, p); // 1 440 (24)

				bean.g[0] = getRand(k, points, funcRelative); // 50
				bean.g[1] = 0.0;
				bean.g[2] = getDev(items, koofs, funcSqr); // 50

				V.add(bean); // 280
			}
		}

		// normalizing weights

		for (int n = 0; n < 3; n++)
		{
			double min = Double.MAX_VALUE;
			double max = Double.MIN_VALUE;
			for (Bean bean : V)
			{
				// 840
				if (!Double.isNaN(bean.g[n]))
				{
					min = Math.min(min, bean.g[n]);
					max = Math.max(max, bean.g[n]);
				}
			}

			if (Math.abs(min - max) > Utils.EPS)
			{
				for (Bean bean : V)
				{
					// 840
					if (!Double.isNaN(bean.g[n]))
					{
						bean.g[n] = (bean.g[n] - min) / (max - min);
					}
				}
			}
		}

		// search for best solution

		double bestDev = Double.MAX_VALUE;
		double bestQ = Double.NaN;
		double bestP = Double.NaN;

		for (Bean bean : V)
		{
			double curDev = Math.pow(bean.g[0], 2) + Math.pow(bean.g[1], 2) + Math.pow(bean.g[2], 2);
			if (curDev < bestDev)
			{
				bestDev = curDev;
				bestQ = bean.q;
				bestP = bean.p;
			}
		}

		// restore
		points = calculateKW(items, bestQ, bestP);
		k = approximate(points, true);
		koofs = copyKQP(k, bestQ, bestP);

		return koofs;
	}
}
