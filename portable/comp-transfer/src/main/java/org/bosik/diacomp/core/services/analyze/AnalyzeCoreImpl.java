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
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.services.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.core.services.analyze.entities.Rate;
import org.bosik.diacomp.core.services.analyze.entities.RateList;
import org.bosik.diacomp.core.services.analyze.entities.PrimeRec;
import org.bosik.diacomp.core.services.analyze.entities.WeightedTimePoint;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

public class AnalyzeCoreImpl implements AnalyzeCore
{
	private static final double	DISC_Q	= 0.0125;
	private static final double	MIN_Q	= 1.50;
	private static final double	MAX_Q	= 5.00 + (DISC_Q / 2);

	private static final double	DISC_P	= 0.05;
	private static final double	MIN_P	= 0.00;
	private static final double	MAX_P	= 0.00 + (DISC_P / 2);

	private class Bean
	{
		public double   p;
		public double   q;
		public double[] g = new double[3];

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
			return Math.pow(1 - (x / y), 2);
		}
	}

	private final double[] TIME_WEIGHTS = new double[Utils.HalfMinPerDay + 1];

	public AnalyzeCoreImpl(double approxFactor)
	{
		// pre-calculation
		for (int i = 0; i < TIME_WEIGHTS.length; i++)
		{
			TIME_WEIGHTS[i] = Math.exp(-approxFactor * Math.pow((double) i / Utils.HalfMinPerDay, 2));
		}
	}

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
		// System.out.println("distance " + time1 + " & " + time2 + " is " + dist);
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

	private static double approximateFull(WeightedTimePoint[] points)
	{
		double summ = 0.0;
		double summWeight = 0.0;
		for (int i = 0; i < points.length; i++)
		{
			if (!Double.isNaN(points[i].getValue()))
			{
				// TODO: optimize
				double curWeight = points[i].getWeight();
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
		if (points.length == 0)
		{
			return null;
		}

		double[] result = new double[Utils.MinPerDay];

		if (highResolution)
		{
			for (int i = 0; i < Utils.MinPerDay; i++)
			{
				result[i] = approximatePoint(points, i);

				// fail-fast shortcut
				if (Double.isNaN(result[i]))
				{
					return null;
				}
			}
		}
		else
		{
			for (int i = 0; i < Utils.MinPerDay / Utils.MinPerHour; i++)
			{
				result[i * Utils.MinPerHour] = approximatePoint(points, i * Utils.MinPerHour);

				// fail-fast shortcut
				if (Double.isNaN(result[i * Utils.MinPerHour]))
				{
					return null;
				}
			}

			// linear interpolation
			for (int left = 0; left < Utils.MinPerDay; left += Utils.MinPerHour) // 0, 60, 120, ...,
																					// 1380
			{
				int right = (left + Utils.MinPerHour) % Utils.MinPerDay;

				for (int m = 1; m < Utils.MinPerHour; m++) // 1..59
				{
					double w = (double) m / Utils.MinPerHour;
					result[left + m] = (1 - w) * result[left] + w * result[right];
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
	private static double getDispersion(double[] recs, WeightedTimePoint[] points, DevFunction func)
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
	 * @param rateList
	 * @param func
	 * @return
	 */
	private static double getError(List<AnalyzeRec> recs, RateList rateList, DevFunction func)
	{
		double result = 0.0;
		int n = 0;
		for (AnalyzeRec rec : recs)
		{
			double k = rateList.getRate(rec.getTime()).getK();
			double q = rateList.getRate(rec.getTime()).getQ();
			double p = rateList.getRate(rec.getTime()).getP();

			double expBs = rec.getBsIn() + k * rec.getCarbs() + p * rec.getProts() - q * rec.getIns();
			double actBs = rec.getBsOut();

			result += func.calculate(expBs, actBs);
			n++;
		}

		if (n > 0)
		{
			result /= n;
		}

		return result;
	}

	/**
	 * O(M) (~1440)
	 * 
	 * @param ks
	 * @param q
	 * @param p
	 * @return
	 */
	private static RateList copyKQP(double[] ks, double q, double p)
	{
		RateList result = new RateList();
		for (int i = 0; i < Utils.MinPerDay; i++)
		{
			result.getRate(i).setK(ks[i]);
			result.getRate(i).setQ(q);
			result.getRate(i).setP(p);
		}
		return result;
	}

	/**
	 * O(N) (~50)
	 * 
	 * @param points
	 * @param index
	 * @return
	 */
	private static WeightedTimePoint[] remove(WeightedTimePoint[] points, int index)
	{
		WeightedTimePoint[] result = new WeightedTimePoint[points.length - 1];

		for (int i = 0; i < index; i++)
		{
			result[i] = points[i];
		}
		for (int i = index; i < result.length; i++)
		{
			result[i] = points[i + 1];
		}

		return result;
	}

	/**
	 * O(N^2) (~2500)
	 * 
	 * @param points
	 * @param bypass
	 * @return
	 */
	private WeightedTimePoint[] filter(WeightedTimePoint[] points, double bypass)
	{
		List<WeightedTimePoint> result = new ArrayList<WeightedTimePoint>(points.length);

		for (int i = 0; i < points.length; i++)
		{
			double average = approximatePoint(remove(points, i), points[i].getTime());
			if (Math.abs(points[i].getValue() - average) / average < bypass)
			{
				result.add(points[i]);
			}
		}

		return result.toArray(new WeightedTimePoint[] {});
	}

	@Override
	public RateList analyze(List<Versioned<DiaryRecord>> records)
	{
		/**
		 * This method assumes the Q and P rates are fixed and K is floating within the day
		 */

		final double ADPTATION_FACTOR = 0.95;
		final double FILTER_BYPASS = 0.5;

		List<PrimeRec> prime = AnalyzeExtracter.extractPrimeRecords(records);
		List<AnalyzeRec> items = AnalyzeExtracter.formatRecords(prime, ADPTATION_FACTOR);

		if (items.isEmpty())
		{
			// throw new IllegalArgumentException("Recs list is empty");
			return null;
		}

		RateList rateList;
		WeightedTimePoint[] points;
		List<Bean> V = new ArrayList<Bean>();
		double[] k;
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
				points = filter(points, FILTER_BYPASS);
				if (points.length > 0)
				{
					k = approximate(points, false); // 72 000 (1 200)
					if (k != null)
					{
						rateList = copyKQP(k, q, p); // 1 440

						bean.g[0] = getDispersion(k, points, funcRelative); // 50
						bean.g[1] = 0.0;
						bean.g[2] = getError(items, rateList, funcSqr); // 50

						V.add(bean); // 280
					}
				}
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
		points = filter(points, FILTER_BYPASS);

		if (points.length > 0)
		{
			k = approximate(points, true);
			if (k != null)
			{
				rateList = copyKQP(k, bestQ, bestP);
				return rateList;
			}
		}

		return null;
	}

	@Override
	public Rate analyzeAverage(List<Versioned<DiaryRecord>> records)
	{
		/*
		 * First implementation: naive, slow
		 */

		RateList rateList = analyze(records);

		if (rateList != null)
		{
			double k = 0;
			double q = 0;
			double p = 0;

			for (int i = 0; i < Utils.MinPerDay; i++)
			{
				k += rateList.getRate(i).getK();
				q += rateList.getRate(i).getQ();
				p += rateList.getRate(i).getP();
			}

			Rate rate = new Rate();
			rate.setK(k / Utils.MinPerDay);
			rate.setQ(q / Utils.MinPerDay);
			rate.setP(p / Utils.MinPerDay);

			return rate;
		}
		else
		{
			return null;
		}
	}
}
