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

		Date fromTime = new Date(2012, 01, 01);
		Date toTime = new Date(2012, 02, 01);
		double adaptation = 0.25; // [0..0.5]
		double valueCarbs = 80.0;
		double valueIns = 10.0;

		r = new Versioned<DiaryRecord>();
		r.setData(new BloodRecord(new Date(2012, 01, 01, 10, 00, 00), 5.0, 0));
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setData(new InsRecord(new Date(2012, 01, 01, 10, 10, 00), valueIns));
		records.add(r);

		r = new Versioned<DiaryRecord>();
		MealRecord meal = new MealRecord(new Date(2012, 01, 01, 10, 40, 00), false);
		meal.add(new FoodMassed("Сахар", 0.0, 0.0, 100.0, 380, valueCarbs));
		r.setData(meal);
		records.add(r);

		r = new Versioned<DiaryRecord>();
		r.setData(new BloodRecord(new Date(2012, 01, 01, 14, 40, 00), 5.0, 0));
		records.add(r);

		source.postRecords(records);

		KoofList koofs = AnalyzeExtracter.analyze(service, source, fromTime, toTime, adaptation);
		Koof koof = koofs.getKoof((10 * 60) + 40);
		double act_x = koof.getK() / koof.getQ();
		double exp_x = valueIns / valueCarbs;

		assertEquals(exp_x, act_x, Utils.EPS);
	}
}
