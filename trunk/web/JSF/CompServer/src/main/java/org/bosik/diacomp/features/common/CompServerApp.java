package org.bosik.diacomp.features.common;

import java.util.HashSet;
import java.util.Set;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;
import org.bosik.diacomp.features.auth.AuthResource;
import org.bosik.diacomp.features.diary.DiaryResource;
import org.bosik.diacomp.features.system.SystemResource;

@ApplicationPath("/")
public class CompServerApp extends Application
{
	@Override
	public Set<Class<?>> getClasses()
	{
		final Set<Class<?>> classes = new HashSet<Class<?>>();
		// register root resource
		classes.add(AuthResource.class);
		classes.add(DiaryResource.class);
		classes.add(SystemResource.class);
		return classes;
	}
}
