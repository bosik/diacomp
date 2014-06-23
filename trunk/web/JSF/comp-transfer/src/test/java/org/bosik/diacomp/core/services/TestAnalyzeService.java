package org.bosik.diacomp.core.services;

import static org.junit.Assert.assertEquals;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDiaryRecord;
import org.bosik.diacomp.core.services.analyze.AnalyzeCore;
import org.bosik.diacomp.core.services.analyze.AnalyzeCoreImpl;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.KoofServiceImpl;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.test.fakes.services.FakeDiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.junit.Before;
import org.junit.Test;

public class TestAnalyzeService
{
	private DiaryService	diaryService;
	private AnalyzeCore		analyzeCore;
	private KoofService		koofService;

	@Before
	public void setUp()
	{
		diaryService = new FakeDiaryService();
		analyzeCore = new AnalyzeCoreImpl(90.0);
		koofService = new KoofServiceImpl(diaryService, analyzeCore, 3650, 0.99);
		koofService.update();
	}

	@Test
	public void testDiaryAnalyze_setA_ok()
	{
		//===========================================================================

		List<Versioned<DiaryRecord>> records = new ArrayList<Versioned<DiaryRecord>>();

		Versioned<DiaryRecord> r;

		double valueCarbs = 80.0;
		double valueIns = 10.0;

		r = new Versioned<DiaryRecord>();
		r.setId("680371a2939446fd9f39f3dc40435f3d");
		r.setData(new BloodRecord(Utils.time(2012, 01, 01, 10, 00, 00), 5.0, 0));
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setId("0c5f4ea3f400459491ed4daed4e41dad");
		r.setData(new InsRecord(Utils.time(2012, 01, 01, 10, 10, 00), valueIns));
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setId("6d2487d5bdcd4d859c148111be000afd");
		MealRecord meal = new MealRecord(Utils.time(2012, 01, 01, 10, 40, 00), false);
		meal.add(new FoodMassed("Сахар", 0.0, 0.0, 100.0, 380, valueCarbs));
		r.setData(meal);
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setId("9b4d7627629e4291b455b23097498624");
		r.setData(new BloodRecord(Utils.time(2012, 01, 01, 14, 40, 00), 5.0, 0));
		records.add(r);

		diaryService.save(records);

		Koof koof = koofService.getKoof((10 * 60) + 40);
		double act_x = koof.getK() / koof.getQ();
		double exp_x = valueIns / valueCarbs;

		assertEquals(exp_x, act_x, Utils.EPS);
	}

	private String readFile(String fileName) throws IOException
	{
		String str = "";
		StringBuffer buf = new StringBuffer();
		InputStream is = getClass().getResourceAsStream(fileName);
		try
		{
			BufferedReader reader = new BufferedReader(new InputStreamReader(is));
			if (is != null)
			{
				while ((str = reader.readLine()) != null)
				{
					buf.append(str + "\n");
				}
			}
		}
		finally
		{
			is.close();
		}

		return buf.toString();
	}

	private List<Versioned<DiaryRecord>> loadRecords(String fileName) throws IOException
	{
		String content = readFile(fileName);
		final Serializer<Versioned<DiaryRecord>> serializer = new SerializerDiaryRecord();
		return serializer.readAll(content);
	}

	@Test
	public void testDiaryAnalyze_setB_ok() throws IOException
	{
		List<Versioned<DiaryRecord>> records = loadRecords("/analyze_data_2.txt");
		KoofList koofs = analyzeCore.analyze(records);

		for (int time = 0; time < Utils.MinPerDay; time++)
		{
			Koof koof = koofs.getKoof(time);
			System.out.println(String.format("%.3f\t%.3f\t%.3f", koof.getK(), koof.getQ(), koof.getP()));
		}
	}
}
