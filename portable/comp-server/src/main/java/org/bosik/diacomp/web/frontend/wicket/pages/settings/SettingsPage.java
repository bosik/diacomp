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
package org.bosik.diacomp.web.frontend.wicket.pages.settings;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.StatelessLink;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.backend.features.user.auth.UserEntity;
import org.bosik.diacomp.web.backend.features.user.auth.UserEntityRepository;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

import java.util.Date;
import java.util.TimeZone;

public class SettingsPage extends MasterPage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private UserInfoService userInfoService;

	@SpringBean
	private UserEntityRepository userEntityRepository;

	@SpringBean
	private AuthService authService;

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		setStatelessHint(true);

		// fetch data
		final TimeZone timeZone = MasterPage.getTimeZone(this);
		final int userId = userInfoService.getCurrentUserId();

		final UserEntity userEntity = userEntityRepository.findById(userId).get();
		final String userEmail = userEntity.getName();
		final Date userDateSignIn = userEntity.getRegistrationDate();
		final Date userDateDeleted = userEntity.getDeletionDate();

		// build UI
		add(new Label("userEmail", userEmail));
		add(new Label("userDateSignUp", Utils.formatDateLocal(timeZone, userDateSignIn)));

		final WebMarkupContainer containerNotDeleted = new WebMarkupContainer("panelAccountNotDeleted");
		containerNotDeleted.setOutputMarkupId(true);
		containerNotDeleted.setVisible(userDateDeleted == null);
		containerNotDeleted.add(new StatelessLink("linkRequestAccountDeletion")
		{
			@Override
			public void onClick()
			{
				authService.scheduleForDeletion(userId);
				setResponsePage(SettingsPage.class);
			}
		});
		add(containerNotDeleted);

		final WebMarkupContainer containerDeleted = new WebMarkupContainer("panelAccountDeleted");
		containerDeleted.setOutputMarkupId(true);
		containerDeleted.setVisible(userDateDeleted != null);
		containerDeleted.add(new Label("userDateDeleted", userDateDeleted != null ? Utils.formatDateLocal(timeZone, userDateDeleted) : ""));
		containerDeleted.add(new StatelessLink("linkCancelAccountDeletion")
		{
			@Override
			public void onClick()
			{
				authService.unscheduleForDeletion(userId);
				setResponsePage(SettingsPage.class);
			}
		});
		add(containerDeleted);
	}
}
