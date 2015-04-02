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
package org.bosik.diacomp.web.frontend.wicket.pages.activation;

import java.util.ArrayList;
import java.util.List;
import org.apache.wicket.ajax.AbstractAjaxTimerBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.StringValue;
import org.apache.wicket.util.time.Duration;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

public class ActivationPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	@SpringBean
	private AuthService			authService;

	public ActivationPage(PageParameters parameters)
	{
		super(parameters);

		FeedbackPanel feedbackPanel = new FeedbackPanel("feedback");
		add(feedbackPanel);

		StringValue parKey = parameters.get("key");

		if (parKey.isEmpty())
		{
			feedbackPanel.error(getString("feedback.error.invalidKey"));
		}
		else
		{
			try
			{
				// activation
				String key = parKey.toString();
				int userId = authService.activate(key);

				// authentication
				String userName = authService.getNameById(userId);
				String userInfo = String.format("%d:%s", userId, userName);
				List<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
				authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
				Authentication authentication = new UsernamePasswordAuthenticationToken(userInfo, "", authorities);
				SecurityContext context = SecurityContextHolder.getContext();
				context.setAuthentication(authentication);

				// UI feedback
				add(new AbstractAjaxTimerBehavior(Duration.seconds(2))
				{
					private static final long	serialVersionUID	= 5004084381865627895L;

					@Override
					protected void onTimer(AjaxRequestTarget target)
					{
						ActivationPage.this.setResponsePage(getApplication().getHomePage());
					}
				});
				feedbackPanel.info(getString("feedback.info.done"));
			}
			catch (NotAuthorizedException e)
			{
				feedbackPanel.error(getString("feedback.error.invalidKey"));
			}
			catch (Exception e)
			{
				e.printStackTrace();
				feedbackPanel.error(getString("feedback.error.invalidKey"));
			}
		}
	}
}
