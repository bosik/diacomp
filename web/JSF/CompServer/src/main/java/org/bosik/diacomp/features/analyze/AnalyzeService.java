package org.bosik.diacomp.features.analyze;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.features.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.features.analyze.entities.KoofList;
import org.bosik.diacomp.features.analyze.entities.WeightedTimePoint;

public class AnalyzeService implements Analyzer
{
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

	private static final double	TIME_WEIGHTS[]	= new double[Utils.HalfMinPerDay + 1];
	{
		final double K = 10.0;
		for (int i = 0; i < TIME_WEIGHTS.length; i++)
		{
			TIME_WEIGHTS[i] = Math.exp(-K * Math.pow(i / Utils.HalfMinPerDay, 2));
		}
	}

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

	private static Double calculateW(AnalyzeRec rec)
	{
		return rec.getWeight();
	}

	private static WeightedTimePoint[] calculateKW(List<AnalyzeRec> recs, double q, double p)
	{
		WeightedTimePoint[] result = new WeightedTimePoint[recs.size()];

		for (int i = 0; i < result.length; i++)
		{
			result[i].setTime(recs.get(i).getTime());
			result[i].setValue(calculateK(recs.get(i), q, p));
			result[i].setWeight(calculateW(recs.get(i)));
		}

		return result;
	}

	private static int timeDistance(int time1, int time2)
	{
		int result = Math.abs(time1 - time2);
		if (result > Utils.HalfMinPerDay)
		{
			result = Utils.MinPerDay - result;
		}
		return result;
	}

	private static double timeWeight(int time1, int time2, double factor)
	{
		int dist = timeDistance(time1, time2);
		return TIME_WEIGHTS[dist];
	}

	private static double approximatePoint(WeightedTimePoint[] points, double factor, int time)
	{
		double summ = 0.0;
		double summWeight = 0.0;
		for (int i = 0; i < points.length; i++)
		{
			if (!Double.isNaN(points[i].getValue()))
			{
				// TODO: optimize
				double curWeight = points[i].getWeight() * timeWeight(time, points[i].getTime(), factor);
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

	private static double[] approximate(WeightedTimePoint[] points, double factor)
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
			for (int i = 0; i < Utils.MinPerDay; i++)
			{
				result[i] = approximatePoint(points, factor, i);
			}
		}

		return result;
	}

	private static double getRand(double[] recs, WeightedTimePoint[] points, DevFunction func)
	{
		double result = 0.0;
		int n = 0;
		for (int i = 0; i < recs.length; i++)
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
	public KoofList analyze(List<AnalyzeRec> recs)
	{
		/**
		 * This method assumes the Q and P koofs are fixed and K is floating within the day
		 */

		if (recs.isEmpty())
		{
			throw new IllegalArgumentException("Recs list is empty");
		}

		final double MIN_Q = 1.50;
		final double MAX_Q = 5.00;
		final double DISC_Q = 0.0125;

		final double MIN_P = 0.00;
		final double MAX_P = 0.00;
		final double DISC_P = 0.05;

		final int APPROX_FACTOR = 90;

		KoofList koofs = new KoofList();
		WeightedTimePoint[] points;
		List<Bean> V = new ArrayList<Bean>();
		double z[];
		DevFunction funcRelative = new RelDev();
		DevFunction funcSqr = new SqrDev();

		for (double q = MIN_Q; q < MAX_Q; q += DISC_Q)
		{
			for (double p = MIN_P; p < MAX_P; p += DISC_P)
			{
				Bean bean = new Bean();
				bean.q = q;
				bean.p = p;

				points = calculateKW(recs, q, p);
				z = approximate(points, APPROX_FACTOR);
				koofs = copyKQP(z, q, p);

				bean.g[0] = getRand(z, points, funcRelative);
				bean.g[1] = 0.0;
				bean.g[2] = getDev(recs, koofs, funcSqr);

				V.add(bean);
			}
		}

		// normalizing weights

		for (int k = 0; k < 3; k++)
		{
			double min = Double.MAX_VALUE;
			double max = Double.MIN_VALUE;
			for (Bean bean : V)
			{
				if (!Double.isNaN(bean.g[k]))
				{
					min = Math.min(min, bean.g[k]);
					max = Math.max(max, bean.g[k]);
				}
			}

			if (Math.abs(min - max) > Utils.EPS)
			{
				for (Bean bean : V)
				{
					if (!Double.isNaN(bean.g[k]))
					{
						bean.g[k] = (bean.g[k] - min) / (max - min);
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
		points = calculateKW(recs, bestQ, bestP);
		z = approximate(points, APPROX_FACTOR);
		koofs = copyKQP(z, bestQ, bestP);

		return koofs;
	}
}
