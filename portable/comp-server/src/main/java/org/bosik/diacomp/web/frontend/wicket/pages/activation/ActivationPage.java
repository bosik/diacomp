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
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
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
						ActivationPage.this.setResponsePage(DiaryPage.class);
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
