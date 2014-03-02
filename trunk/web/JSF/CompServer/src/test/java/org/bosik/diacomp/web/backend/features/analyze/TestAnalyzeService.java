package org.bosik.diacomp.web.backend.features.analyze;

import static org.junit.Assert.assertEquals;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.core.utils.test.MiscUtils;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.analyze.function.AnalyzeExtracter;
import org.bosik.diacomp.web.backend.features.analyze.function.AnalyzeService;
import org.bosik.diacomp.web.backend.features.analyze.function.AnalyzeServiceImpl;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.Koof;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.KoofList;
import org.bosik.diacomp.web.frontend.features.auth.AuthRememberService;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryAuthorizedService;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;
import org.junit.Test;

public class TestAnalyzeService
{
	private AnalyzeService	service	= new AnalyzeServiceImpl();

	@Test
	public void testDiaryAnalyze_setA_ok()
	{
		AuthService authService = new AuthRestClient();

		String login = Config.getLogin();
		String pass = Config.getPassword();
		int apiVersion = Config.getAPICurrent();
		AuthRememberService authRemService = new AuthRememberService(authService, login, pass, apiVersion);

		DiaryService diaryService = new DiaryRestClient();

		DiaryService source = new DiaryAuthorizedService(diaryService, authRemService);

		//===========================================================================

		List<Versioned<DiaryRecord>> records = new LinkedList<Versioned<DiaryRecord>>();

		Versioned<DiaryRecord> r;

		Date fromTime = MiscUtils.date(2012, 01, 01);
		Date toTime = MiscUtils.date(2012, 02, 01);
		double adaptation = 0.25; // [0..0.5]
		double valueCarbs = 80.0;
		double valueIns = 10.0;

		r = new Versioned<DiaryRecord>();
		r.setId("680371a2939446fd9f39f3dc40435f3d");
		r.setData(new BloodRecord(MiscUtils.time(2012, 01, 01, 10, 00, 00), 5.0, 0));
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setId("0c5f4ea3f400459491ed4daed4e41dad");
		r.setData(new InsRecord(MiscUtils.time(2012, 01, 01, 10, 10, 00), valueIns));
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setId("6d2487d5bdcd4d859c148111be000afd");
		MealRecord meal = new MealRecord(MiscUtils.time(2012, 01, 01, 10, 40, 00), false);
		meal.add(new FoodMassed("Сахар", 0.0, 0.0, 100.0, 380, valueCarbs));
		r.setData(meal);
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setId("9b4d7627629e4291b455b23097498624");
		r.setData(new BloodRecord(MiscUtils.time(2012, 01, 01, 14, 40, 00), 5.0, 0));
		records.add(r);

		source.postRecords(records);

		KoofList koofs = AnalyzeExtracter.analyze(service, source, fromTime, toTime, adaptation);
		Koof koof = koofs.getKoof((10 * 60) + 40);
		double act_x = koof.getK() / koof.getQ();
		double exp_x = valueIns / valueCarbs;

		assertEquals(exp_x, act_x, Utils.EPS);
	}
}
