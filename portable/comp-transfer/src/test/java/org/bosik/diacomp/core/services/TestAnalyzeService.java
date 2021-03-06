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
package org.bosik.diacomp.core.services;

import org.junit.Ignore;

@Ignore
public class TestAnalyzeService
{
	//	private DiaryService	diaryService;
	//	private AnalyzeCore		analyzeCore;
	//	private RateService		koofService;
	//
	//	@Before
	//	public void setUp()
	//	{
	//		diaryService = new FakeDiaryService(false);
	//		analyzeCore = new AnalyzeCoreImpl(40.0);
	//		koofService = new KoofServiceImpl(diaryService, analyzeCore, 3650, 0.95);
	//		koofService.update();
	//	}
	//
	//	@Test
	//	@Ignore("this test case is obsolete")
	//	public void testDiaryAnalyze_setA_ok()
	//	{
	//		//===========================================================================
	//
	//		List<Versioned<DiaryRecord>> records = new ArrayList<Versioned<DiaryRecord>>();
	//
	//		Versioned<DiaryRecord> r;
	//
	//		double valueCarbs = 80.0;
	//		double valueIns = 10.0;
	//
	//		r = new Versioned<DiaryRecord>();
	//		r.setId("680371a2939446fd9f39f3dc40435f3d");
	//		r.setData(new BloodRecord(Utils.time(2012, 01, 01, 10, 00, 00), 5.0, 0));
	//		records.add(r);
	//
	//		r = new Versioned<DiaryRecord>();
	//		r.setId("0c5f4ea3f400459491ed4daed4e41dad");
	//		r.setData(new InsRecord(Utils.time(2012, 01, 01, 10, 10, 00), valueIns));
	//		records.add(r);
	//
	//		r = new Versioned<DiaryRecord>();
	//		r.setId("6d2487d5bdcd4d859c148111be000afd");
	//		MealRecord meal = new MealRecord(Utils.time(2012, 01, 01, 10, 40, 00), false);
	//		meal.add(new FoodMassed("Сахар", 0.0, 0.0, 100.0, 380, valueCarbs));
	//		r.setData(meal);
	//		records.add(r);
	//
	//		r = new Versioned<DiaryRecord>();
	//		r.setId("9b4d7627629e4291b455b23097498624");
	//		r.setData(new BloodRecord(Utils.time(2012, 01, 01, 14, 40, 00), 5.0, 0));
	//		records.add(r);
	//
	//		diaryService.save(records);
	//
	//		Rate koof = koofService.getRate((10 * 60) + 40);
	//		double act_x = koof.getK() / koof.getQ();
	//		double exp_x = valueIns / valueCarbs;
	//
	//		assertEquals(exp_x, act_x, Utils.EPS);
	//	}
	//
	//	private String readFile(String fileName) throws IOException
	//	{
	//		String str = "";
	//		StringBuffer buf = new StringBuffer();
	//		InputStream stream = getClass().getResourceAsStream(fileName);
	//		if (stream != null)
	//		{
	//			try
	//			{
	//				BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
	//				while ((str = reader.readLine()) != null)
	//				{
	//					buf.append(str + "\n");
	//				}
	//			}
	//			finally
	//			{
	//				stream.close();
	//			}
	//		}
	//
	//		return buf.toString();
	//	}
	//
	//	private List<Versioned<DiaryRecord>> loadRecords(String fileName) throws IOException
	//	{
	//		String content = readFile(fileName);
	//		final Serializer<Versioned<DiaryRecord>> serializer = new SerializerDiaryRecord();
	//		return serializer.readAll(content);
	//	}
	//
	//	private RateList loadKoofs(String fileName) throws IOException
	//	{
	//		String content = readFile(fileName);
	//		RateList koofs = new RateList();
	//
	//		String[] lines = content.split("\n");
	//		for (int time = 0; time < Utils.MinPerDay; time++)
	//		{
	//			String[] values = lines[time].split("\t");
	//			koofs.getRate(time).setK(Double.parseDouble(values[0]));
	//			koofs.getRate(time).setQ(Double.parseDouble(values[1]));
	//			koofs.getRate(time).setP(Double.parseDouble(values[2]));
	//		}
	//
	//		return koofs;
	//	}
	//
	//	@Test
	//	public void testDiaryAnalyze_setB_ok() throws IOException
	//	{
	//		List<Versioned<DiaryRecord>> records = loadRecords("/analyze_set_1_input.txt");
	//		RateList koofs = analyzeCore.analyze(records);
	//
	//		for (int time = 0; time < Utils.MinPerDay; time++)
	//		{
	//			Rate koof = koofs.getRate(time);
	//			//System.out.println(String.format("%.5f\t%.3f\t%.3f", koof.getK(), koof.getQ(), koof.getP()));
	//		}
	//	}
	//
	//	@Test
	//	public void testDiaryAnalyze_set2_ok() throws IOException
	//	{
	//		List<Versioned<DiaryRecord>> records = loadRecords("/analyze_set_2_input.txt");
	//		RateList koofsExpected = loadKoofs("/analyze_set_2_output.txt");
	//		RateList koofsActual = analyzeCore.analyze(records);
	//
	//		for (int time = 0; time < Utils.MinPerDay; time++)
	//		{
	//			Rate koofExp = koofsExpected.getRate(time);
	//			Rate koofAct = koofsActual.getRate(time);
	//			//System.out.println(String.format("%.5f\t%.3f\t%.3f\t%.5f\t%.3f\t%.3f", koofExp.getK(), koofExp.getQ(),
	//			//		koofExp.getP(), koofAct.getK(), koofAct.getQ(), koofAct.getP()));
	//		}
	//	}
	//
	//	@Test
	//	public void testDiaryAnalyze_set4_ok() throws IOException
	//	{
	//		List<Versioned<DiaryRecord>> records = loadRecords("/analyze_set_4_input.txt");
	//		RateList koofsActual = analyzeCore.analyze(records);
	//		assertNull(koofsActual);
	//	}
	//
	//	@Test
	//	public void testDiaryAnalyze_set5_ok() throws IOException
	//	{
	//		List<Versioned<DiaryRecord>> records = loadRecords("/analyze_set_5_input.txt");
	//		RateList koofsActual = analyzeCore.analyze(records);
	//
	//		for (int time = 0; time < Utils.MinPerDay; time++)
	//		{
	//			Rate koofAct = koofsActual.getRate(time);
	//			//System.out.println(String.format("%.5f\t%.3f\t%.3f", koofAct.getK(), koofAct.getQ(), koofAct.getP())
	//			//		.replace(',', '.'));
	//		}
	//	}
}
