package org.bosik.diacomp.features.analyze;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.features.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.features.analyze.entities.KoofList;
import org.bosik.diacomp.features.analyze.entities.WeightedTimePoint;
import org.bosik.diacomp.utils.Utils;

public class AnalyzeService
{
	private class Bean
	{
		public double	p;
		public double	q;
		public double	g[]	= new double[3];
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
			return (rec.getBsOut() - rec.getBsIn() + rec.getIns() * q - rec.getProts() * p) / rec.getCarbs();
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
			}
		}
	}
}
