package org.bosik.diacomp.web.backend;

import java.util.HashSet;
import java.util.Set;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;
import org.bosik.diacomp.web.backend.features.auth.rest.AuthRestService;
import org.bosik.diacomp.web.backend.features.diary.rest.DiaryRestService;
import org.bosik.diacomp.web.backend.features.system.rest.SystemRestService;

@ApplicationPath("/api/")
public class CompServerApp extends Application
{
	@Override
	public Set<Class<?>> getClasses()
	{
		final Set<Class<?>> classes = new HashSet<Class<?>>();
		// register root resource
		classes.add(AuthRestService.class);
		classes.add(DiaryRestService.class);
		classes.add(SystemRestService.class);
		return classes;
	}
}
