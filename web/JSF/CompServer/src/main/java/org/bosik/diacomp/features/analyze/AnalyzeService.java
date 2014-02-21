package org.bosik.diacomp.features.analyze;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.features.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.features.analyze.entities.KoofList;
import org.bosik.diacomp.features.analyze.entities.WeightedTimePoint;

public class AnalyzeService
{
	private static final double	EPS	= 0.00001;

	private class Bean
	{
		public double	p;
		public double	q;
		public double	g[]	= new double[3];
	}

	private static double calculateK(AnalyzeRec rec, double q, double p)
	{
		if (Math.abs(rec.getCarbs()) > EPS)
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

		KoofList koofs = new KoofList();
		WeightedTimePoint[] points; // = new WeightedTimePoint[recs.size()];
		List<Bean> V = new ArrayList<Bean>();

		for (double q = MIN_Q; q < MAX_Q; q += DISC_Q)
		{
			for (double p = MIN_P; p < MAX_P; p += DISC_P)
			{
				Bean bean = new Bean();
				bean.q = q;
				bean.p = p;

			}
		}
	}
}
