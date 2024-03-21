/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

 Audacity: A Digital Audio Editor

 TransformTests.cpp

 Dmitry Vedenko
 **********************************************************************/

#include <catch2/catch.hpp>

#include "graphics/Point.h"
#include "graphics/Size.h"
#include "graphics/Rect.h"
#include "graphics/Transform.h"

using namespace graphics;

TEST_CASE("PointTest")
{
   const Point zero;

   REQUIRE(zero.x == 0);
   REQUIRE(zero.y == 0);
   REQUIRE((zero * 2).x == 0);

   Point pt = { 1, 2 };

   REQUIRE(pt != zero);
   REQUIRE(pt.x == 1);
   REQUIRE(pt.y == 2);
   REQUIRE(Approx((pt * 2).x) == 2);
   REQUIRE(Approx((pt * 2).y) == 4);
   REQUIRE(Approx((pt / 2).x) == 0.5);
   REQUIRE(Approx((pt / 2).y) == 1);

   pt += pt;

   REQUIRE(Approx(pt.x) == 2);
   REQUIRE(Approx(pt.y) == 4);
   REQUIRE(Approx((pt + pt).x) == 4);
   REQUIRE(Approx((pt + pt).y) == 8);
   REQUIRE(Approx((pt * pt).x) == 4);
   REQUIRE(Approx((pt * pt).y) == 16);
   REQUIRE(Approx((pt / pt).x) == 1);
   REQUIRE(Approx((pt / pt).y) == 1);

   pt *= 2;

   REQUIRE(Approx(pt.x) == 4);
   REQUIRE(Approx(pt.y) == 8);

   pt /= 2;

   REQUIRE(Approx(pt.x) == 2);
   REQUIRE(Approx(pt.y) == 4);

   pt = -pt;

   REQUIRE(Approx(pt.x) == -2);
   REQUIRE(Approx(pt.y) == -4);

   pt += Point { 1, 2 };

   REQUIRE(Approx(pt.x) == -1);
   REQUIRE(Approx(pt.y) == -2);

   pt -= Point { 1, 2 };

   REQUIRE(Approx(pt.x) == -2);
   REQUIRE(Approx(pt.y) == -4);

   pt -= pt;
   REQUIRE(pt == zero);

   REQUIRE(Norm(Point{ 0, 1 }) == 1);
}

TEST_CASE("Transform", "")
{
   Transform identity;

   REQUIRE(!identity.HasTranslation());
   REQUIRE(!identity.HasScale());
   REQUIRE(identity.IsIdentity());

   auto scale = Transform::Scaling(2);

   REQUIRE(!scale.HasTranslation());
   REQUIRE(scale.HasScale());
   REQUIRE(!scale.IsIdentity());

   auto tr = Transform::Translation(2, 4);

   REQUIRE(tr.HasTranslation());
   REQUIRE(!tr.HasScale());
   REQUIRE(!tr.IsIdentity());

   auto combined = tr.Transformed(scale);

   REQUIRE(combined.HasTranslation());
   REQUIRE(combined.HasScale());
   REQUIRE(!combined.IsIdentity());

   auto inverse = combined.Inversed();

   REQUIRE(inverse.HasTranslation());
   REQUIRE(inverse.HasScale());
   REQUIRE(!inverse.IsIdentity());

   REQUIRE(combined.Transformed(inverse).IsIdentity());

   REQUIRE(tr.Apply(Point()).x == 2);
   REQUIRE(tr.Apply(Point()).y == 4);
}
