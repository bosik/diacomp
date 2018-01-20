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
package org.bosik.diacomp.web.frontend.wicket;

import com.googlecode.wickedcharts.wicket6.JavaScriptResourceRegistry;
import de.agilecoders.wicket.core.Bootstrap;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.spring.injection.annot.SpringComponentInjector;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.activation.ActivationPage;
import org.bosik.diacomp.web.frontend.wicket.pages.base.FoodBasePage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.preview.DiaryPreviewPage;
import org.bosik.diacomp.web.frontend.wicket.pages.license.eula.EulaPage;
import org.bosik.diacomp.web.frontend.wicket.pages.license.privacy.PrivacyPolicyPage;
import org.bosik.diacomp.web.frontend.wicket.pages.login.LoginPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.RegisterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.succeed.RegistrationSucceedPage;
import org.bosik.diacomp.web.frontend.wicket.pages.stats.StatsPage;

public class WicketApplication extends WebApplication
{
	@Override
	public Class<? extends WebPage> getHomePage()
	{
		return AboutPage.class;
	}

	@Override
	public RuntimeConfigurationType getConfigurationType()
	{
		return RuntimeConfigurationType.DEPLOYMENT;
	}

	@Override
	public void init()
	{
		super.init();
		getComponentInstantiationListeners().add(new SpringComponentInjector(this));
		getRequestCycleSettings().setGatherExtendedBrowserInfo(true);

		mountPage("/about", AboutPage.class);
		mountPage("/login", LoginPage.class);
		mountPage("/register", RegisterPage.class);
		mountPage("/registered", RegistrationSucceedPage.class);
		mountPage("/activate", ActivationPage.class);
		mountPage("/legal/eula", EulaPage.class);
		mountPage("/legal/privacy", PrivacyPolicyPage.class);

		mountPage("/diary", DiaryPage.class);
		mountPage("/diary/printable", DiaryPreviewPage.class);
		mountPage("/base", FoodBasePage.class);
		mountPage("/stats", StatsPage.class);

		Bootstrap.install(this);

		JavaScriptResourceRegistry.getInstance().setHighchartsReference("https://code.highcharts.com/3.0.2/highcharts.js");
		JavaScriptResourceRegistry.getInstance().setHighchartsExportingReference("https://code.highcharts.com/3.0.2/modules/exporting.js");
	}
}
