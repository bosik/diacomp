/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package org.bosik.diacomp.core.entities.business;

import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class TimedRate implements Serializable
{
	private int    time;
	private double k;
	private double q;
	private double p;

	public TimedRate()
	{
	}

	public TimedRate(int time, Koof c)
	{
		this.time = time;
		this.k = c.getK();
		this.q = c.getQ();
		this.p = c.getP();
	}

	public int getTime()
	{
		return time;
	}

	public void setTime(int time)
	{
		this.time = time;
	}

	public double getK()
	{
		return k;
	}

	public void setK(double k)
	{
		this.k = k;
	}

	public double getQ()
	{
		return q;
	}

	public void setQ(double q)
	{
		this.q = q;
	}

	public double getP()
	{
		return p;
	}

	public void setP(double p)
	{
		this.p = p;
	}

	public static List<TimedRate> readList(String s) throws JSONException
	{
		List<TimedRate> timedRates = new ArrayList<TimedRate>();
		JSONArray json = new JSONArray(s);

		for (int i = 0; i < json.length(); i++)
		{
			JSONObject item = json.getJSONObject(i);

			TimedRate timedRate = new TimedRate();

			timedRate.setTime(item.getInt("time"));
			timedRate.setK(item.getDouble("k"));
			timedRate.setQ(item.getDouble("q"));
			timedRate.setP(item.getDouble("p"));

			timedRates.add(timedRate);
		}

		return timedRates;
	}

	public static String writeList(List<TimedRate> timedRates) throws JSONException
	{
		JSONArray json = new JSONArray();

		for (TimedRate timedRate : timedRates)
		{
			JSONObject item = new JSONObject();

			item.put("time", timedRate.getTime());
			item.put("k", timedRate.getK());
			item.put("q", timedRate.getQ());
			item.put("p", timedRate.getP());

			json.put(item);
		}

		return json.toString();
	}
}
